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
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        programs.sioyek = {
          enable = true;
          config = {
            check_for_updates_on_startup = "1";
            should_launch_new_window = "1";
            "new_command _print" = "${lib.getExe pkgs.yad} --print --type=RAW --filename=%{file_path}";
            "new_command _ocr" = ''${
              lib.getExe (
                pkgs.writers.writePython3Bin "ocr.py"
                  {
                    libraries = with pkgs.python3Packages; [
                      notify-py
                      ocrmypdf
                    ];
                    makeWrapperArgs = [
                      "--prefix"
                      "PATH"
                      ":"
                      "${lib.makeBinPath [ config.programs.sioyek.package ]}"
                    ];
                  }
                  ''
                    import sys
                    import os

                    from notifypy import Notify
                    import ocrmypdf

                    if __name__ == '__main__':
                        file_path = sys.argv[1]
                        new_path = file_path.split('.')[0] + '_ocr.pdf'

                        # TODO: Have a proper GUI window with a progress bar
                        # Notify the user that things are happening
                        notification = Notify()
                        notification.title = "Sioyek OCRmyPDF Script"
                        notification.message = f"Started running OCRmyPDF on {file_path}"
                        notification.send()

                        # Run OCR
                        ocrmypdf.ocr(file_path, new_path)

                        # Open OCR file in sioyek
                        os.system(f'sioyek "{new_path}"')
                  ''
              )
            } %{file_path}'';
          };
          bindings = {
            _print = "<C-p>";
            _ocr = "<A-o>";
          };
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
