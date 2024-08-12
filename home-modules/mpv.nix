{ config, ... }:
{
  programs.mpv =
    let
      fonts = config.stylix.fonts;
    in
    {
      enable = true;
      config = {
        osd-font = fonts.sansSerif.name;
        osd-fractions = true;
        volume = 40;
      };

      profiles = {
        eye-cancer = {
          sharpen = 5;
          osd-font = "Comic Sans MS";
        };
      };

      scriptOpts = {
        console.font = fonts.monospace.name;
        osc.seekbarstyle = "diamond";
      };
    };
}
