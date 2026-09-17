{ config, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  flake.modules.general = {
    nixos.general.environment.persistence.${persistDir}.users.${username}.directories = [
      ".config/strawberry" # Strawberry settings
      ".local/share/strawberry" # Strawberry cache
    ];

    homeManager.general = { pkgs, ... }: {
      home.packages = with pkgs; [ strawberry ];

      xdg.mimeApps.defaultApplications."audio/*" = "org.strawberrymusicplayer.strawberry.desktop";
    };
  };
}
