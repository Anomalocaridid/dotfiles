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
        font.name = builtins.head config.fonts.fontconfig.defaultFonts.sansSerif;
        gtk4.theme = config.gtk.theme;
      };
    };
}
