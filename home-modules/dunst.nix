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
        # NOTE: Match with window manager corner radius
        corner_radius = 12;
        # NOTE: Match with window manager border width
        frame_width = 4;
      };
    };
}
