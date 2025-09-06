{ config, ... }:
let
  inherit (config.flake.meta) username persistDir;
in
{
  unify.modules.general = {
    # Persist Tux Paint saves
    nixos.environment.persistence.${persistDir}.users.${username}.directories = [ ".tuxpaint" ];

    home =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [ tuxpaint ];
      };
  };
}
