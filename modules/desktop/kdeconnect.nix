{ config, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  unify.modules.general = {
    nixos = {
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

    home.services.kdeconnect = {
      enable = true;
      indicator = true;
    };
  };
}
