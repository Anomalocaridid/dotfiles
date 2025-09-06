{ config, ... }:
let
  inherit (config.flake.meta) username persistDir;
in
{
  unify.modules.general = {
    # Persist Bambu Studio login and printer settings
    nixos.environment.persistence.${persistDir}.users.${username}.directories = [
      ".config/OrcaSlicer"
    ];

    home =
      { pkgs, ... }:
      {
        # 3D printer slicer
        home.packages = with pkgs; [ orca-slicer ];
      };
  };
}
