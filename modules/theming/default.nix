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

      qt = rec {
        enable = true;
        style.name = "kvantum";
        platformTheme = style;
      };

      home.pointerCursor =
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
}
