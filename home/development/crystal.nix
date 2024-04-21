{ pkgs, ... }:
{
  home.packages = with pkgs; [ crystal ];

  programs.helix = {
    extraPackages = with pkgs; [ crystalline ];
    languages.language = [
      {
        name = "crystal";
        auto-format = true;
      }
    ];
  };
}
