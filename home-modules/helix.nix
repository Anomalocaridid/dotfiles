{ lib, pkgs, ... }:
{
  programs.helix = {
    enable = true;
    settings = {
      editor = {
        bufferline = "multiple";
        lsp.display-messages = true;
        indent-guides.render = true;
        cursorline = true;
        cursorcolumn = true;
        color-modes = true;
        statusline = {
          left = [
            "mode"
            "spinner"
            "file-name"
            "separator"
            "file-modification-indicator"
          ];
          right = [
            "diagnostics"
            "separator"
            "file-type"
            "separator"
            "selections"
            "separator"
            "position"
            "file-encoding"
          ];
          separator = "";
        };
      };
    };
    extraPackages = with pkgs; [
      nil # nix lsp
    ];
    languages = {
      language-server = {
        # TODO: move to python devshell once Helix updates past 24.7
        # Python linter/formatter lsp
        ruff = {
          command = lib.getExe pkgs.ruff;
          args = [ "server" ];
        };
        rust-analyzer.config.check.command = "clippy";
        # Scheme lsp
        scheme-langserver.command = lib.getExe' pkgs.akkuPackages.scheme-langserver "scheme-langserver";
        # Ruby lsp
        solargraph.config = {
          diagnostics = true;
          formatting = true;
        };
        # JavaScript lsp
        typescript-language-server.config.format.semicolons = "insert";
        unison-language-server = {
          command = lib.getExe pkgs.netcat;
          args = [
            "localhost"
            "5757"
          ];
        };
        zk = {
          command = lib.getExe pkgs.zk;
          args = [ "lsp" ];
        };
      };
      language =
        let
          common.auto-format = true;
          indent4Spaces = common // {
            indent = {
              tab-width = 4;
              unit = "    ";
            };
          };
          withFormatter = command: common // { formatter.command = command; };
        in
        lib.mapAttrsToList (name: value: value // { inherit name; }) {
          bash = indent4Spaces;
          c = indent4Spaces;
          clojure = common;
          cpp = indent4Spaces;
          crystal = common;
          c-sharp = common;
          elixir = common;
          # erlang = common; NOTE: erlang formatter mangles code when auto-format = true
          fortran = withFormatter (lib.getExe pkgs.fprettify);
          go = withFormatter (lib.getExe' pkgs.gotools "goimports");
          haskell = common;
          java = indent4Spaces;
          javascript = common;
          julia = common;
          lua = common;
          markdown = indent4Spaces // {
            # Use zk instead of default lsp
            roots = [ ".zk" ];
            language-servers = [ "zk" ];
          };
          nim = (withFormatter (lib.getExe' pkgs.nim "nimpretty")) // {
            language-servers = [ "nimlsp" ];
          };
          nix = withFormatter (lib.getExe pkgs.nixfmt-rfc-style);
          ocaml = common;
          perl = common; # Also includes raku
          python = common // {
            language-servers = [
              "basedpyright"
              "ruff"
            ];
          };
          r = common;
          racket = common;
          ruby = common;
          scala = common;
          scheme = common // {
            language-servers = [ "scheme-langserver" ];
          };
          sml = (withFormatter (lib.getExe pkgs.smlfmt)) // {
            indent = {
              tab-width = 2;
              unit = "  ";
            };
          };
          typescript = common;
          unison = common // {
            language-servers = [ "unison-language-server" ];
          };
        };
    };
  };
}
