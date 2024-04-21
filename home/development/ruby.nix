{ pkgs, ... }:
{
  home.packages = with pkgs; [
    ruby
    rubyPackages.minitest
  ];

  programs.helix = {
    languages = {
      language-server.solargraph.config = {
        diagnostics = true;
        formatting = true;
      };
      language = [
        {
          name = "ruby";
          auto-format = true;
        }
      ];
    };
    extraPackages = with pkgs; [ rubyPackages.solargraph ];
  };
}
