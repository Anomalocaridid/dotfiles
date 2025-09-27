{ config, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  unify.modules.general = {
    # Persist sioyek document settings
    nixos.environment.persistence.${persistDir}.users.${username}.directories = [
      ".local/share/sioyek"
    ];

    home =
      { lib, pkgs, ... }:
      {
        programs.sioyek = {
          enable = true;
          config = {
            check_for_updates_on_startup = "1";
            should_launch_new_window = "1";
            new_command = "_print ${lib.getExe pkgs.yad} --print --type=RAW --filename=%{file_path}";
          };
          bindings._print = "<C-p>";
        };

        xdg.mimeApps.defaultApplications =
          let
            desktopFile = "sioyek.desktop";
          in
          {
            "application/epub+zip" = desktopFile;
            "application/oxps" = desktopFile;
            "application/pdf" = desktopFile;
          };
      };
  };
}
