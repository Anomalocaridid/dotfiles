{ config, ... }:
{
  services.mako =
    let
      fonts = config.stylix.fonts;
    in
    {
      enable = true;
      catppuccin.enable = true;
      font = "${fonts.sansSerif.name} ${toString fonts.sizes.popups}";
      width = 315;
      height = 200;
      padding = "10";
      margin = "10";
      iconPath = "${config.gtk.iconTheme.package}";
      maxIconSize = 70;
      layer = "overlay";
      borderSize = 5;
      borderRadius = 5;
      defaultTimeout = 5000;
    };
}
