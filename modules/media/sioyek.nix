{
  unify.modules.general.home = {
    programs.sioyek = {
      enable = true;
      config.should_launch_new_window = "1";
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
}
