{ config, ... }:
let
  inherit (config.flake.meta) wm;
in
{
  unify.modules.general.home =
    { config, ... }:
    {
      programs.fuzzel = {
        enable = true;
        settings = {
          main = {
            dpi-aware = "no"; # appears really small otherwise
            icon-theme = config.gtk.iconTheme.name;
            terminal = "xterm";
            # Match default vertical-pad
            horizontal-pad = 8;
          };
          border = {
            width = wm.borderWidth;
            radius = wm.windowCornerRadius;
          };
        };
      };
    };
}
