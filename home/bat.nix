{ pkgs, ... }:
{
  programs.bat = {
    enable = true;
    config = {
      theme = "cyberpunk-neon";
    };
    themes = {
      cyberpunk-neon = builtins.readFile ../config/bat/themes/cyberpunk-neon.tmTheme;
    };
    extraPackages = with pkgs.bat-extras; [
      batdiff
      batgrep
      batman
      batpipe
    ];
  };
}
