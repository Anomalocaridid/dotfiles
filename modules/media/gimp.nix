{ config, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  flake.modules = {
    # GIMP settings
    nixos.general.environment.persistence.${persistDir}.users.${username}.directories = [
      ".config/GIMP"
    ];

    homeManager.general = { pkgs, ... }: {
      home.packages = with pkgs; [ gimp3-with-plugins ];
      xdg.configFile."GIMP/3.0/gimprc".text = # scheme
        ''
          (theme "System")
        '';
    };
  };
}
