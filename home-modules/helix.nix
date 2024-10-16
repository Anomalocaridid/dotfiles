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
        rust-analyzer.config.check.command = "clippy";
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
      language = [
        {
          name = "bash";
          auto-format = true;
          indent = {
            tab-width = 4;
            unit = "    ";
          };
        }
        {
          name = "c";
          auto-format = true;
          indent = {
            tab-width = 4;
            unit = "    ";
          };
        }
        {
          name = "clojure";
          auto-format = true;
        }
        {
          name = "cpp";
          auto-format = true;
          indent = {
            tab-width = 4;
            unit = "    ";
          };
        }
        {
          name = "crystal";
          auto-format = true;
        }
        {
          name = "c-sharp";
          auto-format = true;
        }
        {
          name = "elixir";
          auto-format = true;
        }
        {
          name = "haskell";
          auto-format = true;
        }
        {
          name = "java";
          auto-format = true;
          indent = {
            tab-width = 4;
            unit = "    ";
          };
        }
        {
          name = "javascript";
          auto-format = true;
        }
        {
          name = "julia";
          auto-format = true;
        }
        {
          name = "lua";
          auto-format = true;
        }
        {
          name = "markdown";
          auto-format = true;
          indent = {
            tab-width = 4;
            unit = "    ";
          };
          # Use zk instead of default lsp
          roots = [ ".zk" ];
          language-servers = [ "zk" ];
        }
        {
          name = "nim";
          auto-format = true;
          language-servers = [ "nimlsp" ];
          formatter.command = lib.getExe' pkgs.nim "nimpretty";
        }
        {
          name = "nix";
          auto-format = true;
          formatter.command = lib.getExe pkgs.nixfmt-rfc-style;
        }
        {
          name = "python";
          auto-format = true;
        }
        {
          name = "r";
          auto-format = true;
        }
        {
          name = "ruby";
          auto-format = true;
        }
        {
          name = "sml";
          auto-format = true;
          formatter.command = lib.getExe pkgs.smlfmt;
          indent = {
            tab-width = 2;
            unit = "  ";
          };
        }
        {
          name = "unison";
          auto-format = true;
          language-servers = [ "unison-language-server" ];
        }
      ];
    };
  };
}
