{ pkgs, ... }:
{
  home.packages = with pkgs; [ nodejs ];

  programs.helix = {
    extraPackages = with pkgs; [ nodePackages.typescript-language-server ];

    languages = {
      language-server.typescript-language-server.config.format.semicolons = "insert";
      language = [
        {
          name = "javascript";
          auto-format = true;
        }
      ];
    };
  };
}
