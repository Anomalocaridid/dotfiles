{ config, ... }:
let
  inherit (config.flake.meta) wm;
in
{
  flake.meta.selector = rec {
    apps = "anyrun";
    # NOTE: needed to access home manager config
    stdin = config: "${apps} --plugins ${config.programs.anyrun.package}/lib/libstdin.so";
  };

  unify.modules.general.home =
    { config, pkgs, ... }:
    {
      programs.anyrun =
        let
          anyrunPkg = config.programs.anyrun.package;
        in
        {
          enable = true;
          config = {
            plugins = map (plugin: "${anyrunPkg}/lib/lib${plugin}.so") [
              "applications"
              "rink"
              "niri_focus"
              "symbols"
              "nix_run"
              "dictionary"
            ];
            y.fraction = 0.25;
            showResultsImmediately = true;
            closeOnClick = true;
          };
          # Default is "", which gets rid of default theming
          # TODO: remove when this is made default
          extraCss = # css
            ''
              /* Import default style so it is not entirely overwritten */
              @import "file://${anyrunPkg.src}/anyrun/res/style.css";

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
