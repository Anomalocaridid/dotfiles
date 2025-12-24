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
                pkgs.writeShellApplication {
                  name = "ocr.sh";
                  runtimeInputs = with pkgs; [
                    # handlr-regex
                    ghostty
                    ocrmypdf
                    config.programs.sioyek.package
                    libnotify
                  ];
                  text = ''
                    readonly file_path="''${1/%.pdf/_ocr.pdf}"
                    # Run OCR and show output in terminal window
                    # handlr launch x-scheme-handler/terminal -- --class='com.terminal.ocrmypdf' -e ocrmypdf "$1" "$file_path" && sioyek "$file_path"
                    # TODO: replace with handlr-regex once blocking flag is added
                    ghostty --class='com.terminal.ocrmypdf' -e ocrmypdf "$1" "$file_path"
                    # Open OCR PDF file in sioyek
                    sioyek --reuse-window "$file_path"
                    notify-send --icon=document-viewer "Sioyek OCRmyPDF Script" "Finished processing '$1'"
                  '';
                }
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
