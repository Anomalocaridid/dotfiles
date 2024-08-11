{ pkgs, ... }:
{
  home.packages = with pkgs; [ unison-ucm ];

  programs.helix = {
    extraPackages = with pkgs; [
      netcat-gnu # Required for lsp
    ];

    languages = {
      language-server = {
        unison-language-server = {
          command = "netcat";
          args = [
            "localhost"
            "5757"
          ];
        };
      };
      language = [
        {
          name = "unison";
          auto-format = true;
          indent = {
            tab-width = 4;
            unit = "    ";
          };
          scope = "scope.unison";
          injection-regex = "unison";
          file-types = [ "u" ];
          shebangs = [ ];
          roots = [ ];
          comment-token = "--";
          language-servers = [ "unison-language-server" ];
        }
      ];
    };
  };
}
