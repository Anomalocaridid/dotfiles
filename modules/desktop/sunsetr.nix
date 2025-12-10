{ config, inputs, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  unify.modules.general = {
    # sunsetr location info
    # NOTE: contains private location data
    nixos.environment.persistence.${persistDir}.users.${username}.files = [
      ".config/sunsetr/geo.toml"
    ];

    home =
      { pkgs, ... }:
      let
        dayTemp = 6500;
        dayGamma = 100;
        common = {
          #[Backend]
          backend = "auto";

          #[Smoothing]
          smoothing = true;
          startup_duration = 0.5;
          shutdown_duration = 0.5;
          adaptive_interval = 1;
        };
        sunsetrConfig =
          settings:
          (inputs.nixago.lib.${pkgs.stdenv.hostPlatform.system}.make {
            data = common // settings;
            output = "sunsetr.toml";
          }).configFile;
      in
      {
        xdg.configFile = {
          "sunsetr/sunsetr.toml".source = sunsetrConfig {
            #[Backend]
            transition_mode = "geo";

            #[Time-based config]
            night_temp = 3300;
            day_temp = dayTemp;
            night_gamma = 90;
            day_gamma = dayGamma;
            update_interval = 60;
          };
          "sunsetr/presets/day/sunsetr.toml".source = sunsetrConfig {
            #[Backend]
            transition_mode = "static";

            #[Static configuration]
            static_temp = dayTemp; # Neutral daylight
            static_gamma = dayGamma; # Full brightness
          };
        };
      };
  };
}
