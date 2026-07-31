{
  unify = {
    modules.general.home = {
      enable = true;
      gtk.enable = true;
    };

    # TODO: move cursor config to general instead of doing this
    modules.htpc.home.home.pointerCursor.enable = false;

    home =
      { config, pkgs, ... }:
      {
        home.pointerCursor =
          let
            palette = config.catppuccin.sources.parsedPalette;
          in
          {
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
