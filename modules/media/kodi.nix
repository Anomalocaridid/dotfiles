# Setup to run Kodi as a standalone appliance
{ config, ... }:
let
  inherit (config.flake.meta) username;
in
{
  unify.modules.htpc = {
    nixos =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        users = {
          users.kodi = {
            group = "kodi";
            # Allow kodi user access to keyboards
            extraGroups = [ "input" ];
            isSystemUser = true;
            home = "/var/lib/kodi";
            createHome = true;
          };
          groups.kodi = { };
        };

        services.greetd = {
          enable = true;
          settings = {
            initial_session = {
              command = "${lib.getExe' pkgs.kodi-gbm "kodi-standalone"}";
              user = "kodi";
            };

            # Add method to access a tty to prevent being locked out if something breaks
            default_session.command = "${lib.getExe pkgs.tuigreet} --cmd ${
              lib.getExe config.users.users.${username}.shell
            }";
          };
        };
      };
  };
}
