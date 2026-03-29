{
  unify.modules.general.home = {
    xdg.mimeApps.defaultApplications =
      let
        desktopFile = "Helix.desktop";
      in
      {
        "application/toml" = desktopFile;
        "application/x-yaml" = desktopFile;
        "text/*" = desktopFile;
      };

    programs.helix = {
      enable = true;
      settings = {
        # Use terminal file picker
        # NOTE: Assumes yazi is the file manager
        keys.normal.C-y = [
          ":sh rm -f /tmp/unique-ca1ea106"
          '':insert-output yazi "%{buffer_name}" --chooser-file=/tmp/unique-ca1ea106''
          '':sh printf "\x1b[?1049h\x1b[?2004h" > /dev/tty''
          ":open %sh{cat /tmp/unique-ca1ea106}"
          ":redraw"
          ":set mouse false"
          ":set mouse true"
        ];

        editor = {
          bufferline = "multiple";
          lsp.display-messages = true;
          indent-guides.render = true;
          cursorline = true;
          cursorcolumn = true;
          color-modes = true;
          statusline = {
            left = [
              "mode"
              "spinner"
              "file-name"
              "separator"
              "file-modification-indicator"
            ];
            right = [
              "diagnostics"
              "separator"
              "file-type"
              "separator"
              "selections"
              "separator"
              "position"
              "file-encoding"
            ];
            separator = "";
          };
        };
      };
    };
  };
}
