{ config, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  flake.modules = {
    nixos.general = {
      # Persist KDE Connect state
      environment.persistence.${persistDir}.users.${username}.directories = [
        ".config/kdeconnect"
      ];

      # Open firewall ports, but do not install redundant package
      programs.kdeconnect = {
        enable = true;
        package = null;
      };
    };

    homeManager.general.services.kdeconnect = {
      enable = true;
      indicator = true;
    };
  };
}
