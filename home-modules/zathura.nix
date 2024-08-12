{ config, ... }:
{
  programs.zathura = {
    enable = true;
    options =
      let
        fonts = config.stylix.fonts;
      in
      {
        font = "${fonts.sansSerif.name} ${toString fonts.sizes.applications}";
        recolor = false;
        selection-clipboard = "clipboard";
        statusbar-home-tilde = true;
        window-title-basename = true;
      };
  };
}
