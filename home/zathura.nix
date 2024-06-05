{ config, lib, ... }:
{
  programs.zathura = {
    enable = true;
    catppuccin.enable = true;
    options =
      let
        fonts = config.stylix.fonts;
      in
      {
        font = "${fonts.sansSerif.name} ${toString fonts.sizes.applications}";
        recolor = lib.mkForce false;
        selection-clipboard = "clipboard";
        statusbar-home-tilde = true;
        window-title-basename = true;
      };
  };
}
