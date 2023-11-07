{ lib, pkgs, config, osConfig, ... }: {
  # Qt theme defined in configuration.nix because it works better at system level
  gtk = {
    enable = true;
    catppuccin.enable = true;
    # Stylix currently sets more things than ctp-nix by default
    cursorTheme = config.stylix.cursor;
    iconTheme = {
      name = "Sweet-Rainbow";
      package = pkgs.custom.candy-icons;
    };
  };

  home = {
    # fallback icon theme
    packages = with pkgs; [ gnome.adwaita-icon-theme ];
  };

  # For some reason, font settings are not carried over automatically
  stylix = {
    fonts = osConfig.stylix.fonts;
    # Use catppuccin cursor
    # Code borrowed from ctp-nix
    cursor =
      let
        mkUpper = str:
          with builtins;
          (lib.toUpper (substring 0 1 str)) + (substring 1 (stringLength str) str);
        cfg = config.gtk.catppuccin.cursor;
        flavourUpper = mkUpper cfg.flavour;
        accentUpper = mkUpper cfg.accent;
      in
      {
        name = "Catppuccin-${flavourUpper}-${accentUpper}-Cursors";
        package = pkgs.catppuccin-cursors.${cfg.flavour + accentUpper};
      };
  };

  catppuccin = {
    flavour = osConfig.catppuccin.flavour;
    accent = "mauve";
  };

  # Required for btop theme
  xdg.enable = true;
}
