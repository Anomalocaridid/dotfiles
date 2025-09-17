{ config, pkgs, ... }:
let
  inherit (config.flake.meta) wm;
in
{
  flake.meta.selector = rec {
    menu = "anyrun";
    stdin = "${menu} --plugins ${pkgs.anyrun}/lib/libstdin.so";
  };

  unify.modules.general.home =
    { config, pkgs, ... }:
    {
      programs.anyrun = {
        enable = true;
        config = {
          plugins =
            let
              pkg = config.programs.anyrun.package;
            in
            [
              "${pkg}/lib/libapplications.so"
              "${pkg}/lib/libniri_focus.so"
            ];
          y.fraction = 0.25;
          showResultsImmediately = true;
          closeOnClick = true;
        };
        # Default is "", which gets rid of default theming
        # TODO: remove when this is made default
        extraCss = # css
          ''
            /* Import default style so it is not overwritten */
            @import "file://${config.programs.anyrun.package.src}/anyrun/res/style.css";

            /* Tweak styling to fit system styling */
            box.main {
              border-width: ${toString wm.borderWidth}px;
              border-radius: ${toString wm.windowCornerRadius}px;
              /* Use window manager's shadows */
              box-shadow: unset;
              margin: unset;
            }
          '';
      };
    };
}
