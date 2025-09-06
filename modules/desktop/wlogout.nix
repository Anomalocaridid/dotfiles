{ config, ... }:
let
  inherit (config.flake.meta) wm;
in
{
  unify.modules.general.home =
    { config, lib, ... }:
    {
      programs.wlogout.enable = true;

      catppuccin.wlogout.extraStyle =
        let
          override =
            entry: icon: vertBorder: horiBorder:
            #css
            ''
              #${entry} {
                /* Customize icon */
                background-image: url("${config.gtk.iconTheme.package}/share/icons/${config.gtk.iconTheme.name}/apps/scalable/${icon}.svg");
                /* Customize outer border */
                ${lib.optionalString (horiBorder != null)
                  #css
                  ''
                    border-${vertBorder}-${horiBorder}-radius: ${toString wm.windowCornerRadius};
                    border-${horiBorder}-width: ${toString wm.borderWidth}px;''
                }
                border-${vertBorder}-width: ${toString wm.borderWidth}px;
              }
            '';
        in
        # css
        ''
          ${override "lock" "system-lock-screen" "top" "left"}
          ${override "hibernate" "system-suspend-hibernate" "bottom" "left"}
          ${override "logout" "system-log-out" "top" null}
          ${override "shutdown" "system-shutdown" "bottom" null}
          ${override "suspend" "system-suspend" "top" "right"}
          ${override "reboot" "system-reboot" "bottom" "right"}
        '';
    };
}
