{ config, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  unify.modules.general = {
    nixos.environment.persistence.${persistDir}.users.${username}.directories = [
      ".config/strawberry" # Strawberry settings
      ".local/share/strawberry" # Strawberry cache
    ];

    home =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          strawberry
        ];

        xdg.mimeApps.defaultApplications."audio/*" = "org.strawberrymusicplayer.strawberry.desktop";
      };
  };
}
