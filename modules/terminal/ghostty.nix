{
  unify.modules.general.home =
    { config, ... }:
    {
      xdg.mimeApps.defaultApplications."x-scheme-handler/terminal" = "com.mitchellh.ghostty.desktop";

      programs.ghostty = {
        enable = true;
        settings = {
          font-family = builtins.head config.fonts.fontconfig.defaultFonts.monospace;
          font-size = 11;
          theme = "catppuccin-${config.catppuccin.flavor}";
          window-decoration = false;
          background-opacity = 0.9;
          window-padding-color = "extend";
          confirm-close-surface = false;
          font-feature = [
            "ss09" # >>= <<= ||= |=
            "cv25" # .-
            "cv26" # :-
            "cv32" # .=
            "cv27" # []
            "cv28" # {. .}
            "ss06" # \\
            "ss07" # =~ !~
          ];
        };
      };
    };
}
