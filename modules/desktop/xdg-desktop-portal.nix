{
  unify.modules.general = {
    nixos =
      { pkgs, ... }:
      {
        # Assumes niri is the window manager
        xdg.portal = {
          enable = true;
          xdgOpenUsePortal = true;
          extraPortals = with pkgs; [
            xdg-desktop-portal-termfilechooser
            xdg-desktop-portal-gtk
            xdg-desktop-portal-gnome
          ];

          config.common = {
            "org.freedesktop.impl.portal.FileChooser" = "termfilechooser";
            "org.freedesktop.impl.portal.ScreenCast" = "gnome";
            "org.freedesktop.impl.portal.Screenshot" = "gnome";
          };
        };
      };

    home =
      { lib, pkgs, ... }:
      {
        # Ensure GTK programs use termfilechooser
        home.sessionVariables.GDK_USE_PORTAL = 1;

        # Ensure Qt programs use termfilechooser
        qt = lib.genAttrs [ "qt5ctSettings" "qt6ctSettings" ] (_: {
          Appearance.standard_dialogs = "xdgdesktopportal";
        });

        # Assumes yazi is the file manager
        # TODO: For some reason, this does not work when using `handlr launch x-scheme-handler/terminal`
        xdg.configFile."xdg-desktop-portal-termfilechooser/config".source =
          (pkgs.formats.ini { }).generate "config"
            {
              filechooser = {
                cmd = "${pkgs.xdg-desktop-portal-termfilechooser}/share/xdg-desktop-portal-termfilechooser/yazi-wrapper.sh";
                default_dir = "$HOME";
                env = ''TERMCMD='ghostty --title="terminal filechooser" -e' '';
                open_mode = "suggested";
                save_mode = "last";
              };
            };
      };
  };
}
