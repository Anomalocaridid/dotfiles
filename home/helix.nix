{ lib, ... }: {
  programs.helix = {
    enable = true;
    catppuccin.enable = true;
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
          # center = [];
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
    languages.language = lib.concatLists [
      # Languages that just need auto-format = true
      (map
        (name: {
          name = name;
          auto-format = true;
        })
        [
          "haskell"
          "javascript"
          "lua"
          "nix"
          "python"
        ]
      )
      # Languages that need auto-format and indent
      (map
        (name: {
          name = name;
          auto-format = true;
          indent = {
            tab-width = 4;
            unit = "    ";
          };
        })
        [
          "bash"
          "c"
          "cpp"
          "java"
          "markdown"
          "unison"
        ]
      )
      [
        {
          name = "bash";
          formatter.command = "shfmt";
        }
        {
          name = "java";
          # TODO: replace with jdtls if it gets fixed
          language-server.command = "java-language-server";
          formatter.command = "google-java-format";
        }
        {
          name = "javascript";
          config.format.semicolons = "insert";
        }
        {
          name = "nix";
          formatter.command = "nixpkgs-fmt";
        }
        {
          name = "markdown";
          roots = [ ".zk" ];
          language-server = {
            command = "zk";
            args = [ "lsp" ];
          };
        }
        {
          name = "unison";
          scope = "scope.unison";
          injection-regex = "unison";
          file-types = [ "u" ];
          shebangs = [ ];
          roots = [ ];
          comment-token = "--";
          language-server = {
            command = "netcat";
            args = [ "localhost" "5757" ];
          };
        }
      ]
    ];
  };
}
