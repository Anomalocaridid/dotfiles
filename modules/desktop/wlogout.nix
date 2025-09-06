{
  unify.modules.general.home =
    { config, lib, ... }:
    {
      programs.wlogout.enable = true;

      catppuccin.wlogout.extraStyle =
        let
          niriSettings = config.programs.niri.settings;
          borderRadius = (builtins.elemAt niriSettings.window-rules 0).geometry-corner-radius;
          borderWidth = "${toString niriSettings.layout.border.width}px";
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
                    border-${vertBorder}-${horiBorder}-radius: ${
                      toString (builtins.ceil borderRadius."${vertBorder}-${horiBorder}")
                    };
                    border-${horiBorder}-width: ${borderWidth};''
                }
                border-${vertBorder}-width: ${borderWidth};
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
