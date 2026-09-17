{ config, ... }:
let
  inherit (config.flake.meta) username persistDir;
in
{
  flake.modules = {
    # Persist Tux Paint saves
    nixos.general.environment.persistence.${persistDir}.users.${username}.directories = [ ".tuxpaint" ];

    homeManager.general = { pkgs, ... }: {
      home.packages = with pkgs; [ tuxpaint ];
    };
  };
}
