{
  unify.modules.general.home =
    {
      config,
      lib,
      pkgs,
      osConfig,
      ...
    }:
    {
      gtk.enable = true;

      xresources = {
        # Rename file to avoid needing to manually run xrdb in a Wayland session
        path = "${config.home.homeDirectory}/.Xdefaults";
        properties."*TkTheme" = "gtkTtk";
      };

      home = {
        packages = [
          pkgs.custom.ttk-theme-chooser
        ];
        sessionVariables."TCLLIBPATH" = pkgs.custom.gtkTtk;

        pointerCursor =
          let
            palette = config.catppuccin.sources.parsedPalette;
          in
          {
            enable = true;
            gtk.enable = true;
            name = "Breeze_Hacked";
            size = 24;
            package = pkgs.breeze-hacked-cursor-theme.override {
              accentColor = "${palette.${config.catppuccin.accent}.hex}";
              baseColor = "${palette.base.hex}";
              borderColor = "${palette.base.hex}";
              logoColor = "${palette.text.hex}";
            };
          };
      };
    };
}
