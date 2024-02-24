{ pkgs, config, osConfig, inputs, ... }: {
  # Qt theme defined in configuration.nix because it works better at system level
  gtk = {
    enable = true;
    catppuccin.enable = true;
    cursorTheme = config.stylix.cursor;
    iconTheme = {
      name = "Sweet-Rainbow";
      package = pkgs.sweet-folders;
    };
  };

  home = {
    packages = with pkgs; [
      # fallback icon theme
      gnome.adwaita-icon-theme
      # needed for Sweet Folders
      candy-icons
      # Tools for making catppuccin ports
      inputs.catppuccin-toolbox.packages.${pkgs.system}.catwalk
      inputs.catppuccin-toolbox.packages.${pkgs.system}.puccinier
      inputs.catppuccin-toolbox.packages.${pkgs.system}.whiskers
    ];
  };

  # Define here instead of globally because there is no global accent option in ctp-nix
  stylix.cursor =
    let
      palette = pkgs.custom.catppuccin-palette.${config.catppuccin.flavour};
    in
    {
      name = "Breeze_Hacked";
      package = pkgs.breeze-hacked-cursor-theme.override {
        accentColor = "#${palette.${config.catppuccin.accent}.hex}";
        baseColor = "#${palette.base.hex}";
        borderColor = "#${palette.base.hex}";
        logoColor = "#${palette.text.hex}";
      };
    };

  catppuccin = {
    flavour = osConfig.catppuccin.flavour;
    accent = "mauve";
  };

  # Required for btop theme
  xdg.enable = true;
}
