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
      settings.editor = {
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
}
