{ config, ... }:
let
  inherit (config.flake.meta) username persistDir;
in
{
  flake.modules = {
    # Persist Bambu Studio login and printer settings
    nixos.general.environment.persistence.${persistDir}.users.${username}.directories = [
      ".config/OrcaSlicer"
    ];

    homeManager.general = { pkgs, ... }: {
      # 3D printer slicer
      home.packages = with pkgs; [ orca-slicer ];
    };
  };
}
