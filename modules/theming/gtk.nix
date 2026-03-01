{
  unify.modules.general.home =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {

      gtk = {
        enable = true;
        # NOTE: this catppuccin gtk theme does not yet use the names of flavors
        theme = {
          name = "Catppuccin-GTK-${lib.toSentenceCase config.catppuccin.accent}-Dark";
          package = pkgs.magnetic-catppuccin-gtk.override { accent = [ config.catppuccin.accent ]; };
        };
      };

      # Manually link GTK theme accents
      xdg.configFile."gtk-4.0" = {
        source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/";
        # Do not interfere with other links in the same directory
        recursive = true;
      };
    };
}
