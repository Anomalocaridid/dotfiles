{ config, ... }:
let
  inherit (config.flake.meta) username persistDir;
in
{
  flake.modules = {
    nixos.general.environment.persistence.${persistDir}.users.${username} = {
      directories = [
        "Sync" # Files synced by Syncthing
        ".local/state/syncthing" # Syncthing settings
      ];
      files = [
        ".config/syncthingtray.ini" # Syncthingtray settings NOTE: contains api key for Syncthing
      ];
    };

    homeManager.general.services.syncthing = {
      enable = true;
      tray.enable = true;
    };
  };
}
