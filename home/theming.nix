{ pkgs, osConfig, ... }: {
  # Qt theme defined in configuration.nix because it works better at system level
  gtk = {
    enable = true;
    catppuccin.enable = true;
    iconTheme = {
      name = "Sweet-Rainbow";
      package = pkgs.custom.candy-icons;
    };
  };

  home = {
    pointerCursor = {
      name = "Breeze_Hacked";
      package = pkgs.custom.breeze-hacked-cursor;
      gtk.enable = true;
      x11.enable = true;
    };
    # fallback icon theme
    packages = with pkgs; [ gnome.adwaita-icon-theme ];
  };

  # For some reason, font settings are not carried over automatically
  stylix.fonts = osConfig.stylix.fonts;

  catppuccin = {
    flavour = osConfig.catppuccin.flavour;
    accent = "mauve";
  };

  # Required for btop theme
  xdg.enable = true;
}
