{ config, inputs, ... }:
let
  inherit (config.flake.meta) username;
in
{
  unify.modules.general.nixos =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      services.displayManager.ly = {
        enable = true;
        settings = {
          animation = "colormix";
          asterisk = "0x2022"; # •
          bigclock = "en";
          clear_password = true;
          colormix_col1 = "0x0006"; # Magenta
          colormix_col2 = "0x0005"; # Blue
          colormix_col3 = "0x0007"; # Cyan
          hide_borders = true;
        };
      };

      # Set default session
      environment.etc."ly/save.ini".source =
        (inputs.nixago.lib.${pkgs.system}.make {
          output = "save.ini";
          data.globalSection = {
            user = username;
            session_index = 2;
          };
          format = "iniWithGlobalSection";
        }).configFile;

      # Ensure services start properly
      systemd.services.display-manager.environment.XDG_CURRENT_DESKTOP = "X-NIXOS-SYSTEMD-AWARE";
    };
}
