{ config, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  unify.modules.general = {
    # GIMP settings
    nixos.environment.persistence.${persistDir}.users.${username}.directories = [ ".config/GIMP" ];

    home =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [ gimp3-with-plugins ];
        xdg.configFile."GIMP/3.0/gimprc".text = # scheme
          ''
            (theme "System")
          '';
      };
  };
}
