{
  unify.modules.general.home = {
    xdg.mimeApps.defaultApplications."video/*" = "mpv.desktop";

    programs.mpv = {
      enable = true;
      config = {
        osd-fractions = true;
        volume = 40;
      };

      profiles = {
        eye-cancer = {
          sharpen = 5;
          osd-font = "Comic Sans MS";
        };
      };

      scriptOpts.osc.seekbarstyle = "diamond";
    };
  };
}
