{ config, ... }:
{
  services.dunst =
    let
      fonts = config.stylix.fonts;
      niriSettings = config.programs.niri.settings;
      outerGap = niriSettings.layout.gaps - niriSettings.layout.border.width;
    in
    {
      enable = true;
      settings.global = {
        font = "${fonts.sansSerif.name} ${toString fonts.sizes.popups}";
        icon_theme = config.gtk.iconTheme.name;
        enable_recursive_icon_lookup = true;
        dmenu = "fuzzel --dmenu --prompt='dunst'";
        # NOTE: Depends on window-rule order, chooses corner that it should match up with
        corner_radius = builtins.ceil (builtins.elemAt niriSettings.window-rules 0)
          .geometry-corner-radius.top-right;
        frame_width = niriSettings.layout.border.width;
        offset = "(${toString outerGap}, ${toString outerGap})";
      };
    };
}
