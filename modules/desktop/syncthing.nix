{ config, ... }:
let
  inherit (config.flake.meta) username persistDir;
in
{
  unify.modules.general = {
    nixos.environment.persistence.${persistDir}.users.${username} = {
      directories = [
        "Sync" # Files synced by Syncthing
        ".local/state/syncthing" # Syncthing settings
      ];
      files = [
        ".config/syncthingtray.ini" # Syncthingtray settings NOTE: contains api key for Syncthing
      ];
    };

    home.services.syncthing = {
      enable = true;
      tray.enable = true;
    };
  };
}
