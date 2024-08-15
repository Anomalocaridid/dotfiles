{ config, ... }:
{
  services.dunst =
    let
      fonts = config.stylix.fonts;
    in
    {
      enable = true;
      settings.global = {
        font = "${fonts.sansSerif.name} ${toString fonts.sizes.popups}";
        icon_theme = config.gtk.iconTheme.name;
        enable_recursive_icon_lookup = true;
        dmenu = "fuzzel --dmenu --prompt='dunst'";
        corner_radius = 3;
      };
    };
}
