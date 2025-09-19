{ config, ... }:
let
  inherit (config.flake.meta) wm selector;
in
{
  unify.modules.general.home =
    { config, ... }:
    {
      services.dunst = {
        enable = true;
        settings.global = {
          icon_theme = config.gtk.iconTheme.name;
          enable_recursive_icon_lookup = true;
          dmenu = selector.stdin config;
          corner_radius = wm.windowCornerRadius;
          frame_width = wm.borderWidth;
          offset = "(${toString wm.gapMinusBorder}, ${toString wm.gapMinusBorder})";
        };
      };
    };
}
