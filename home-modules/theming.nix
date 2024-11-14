{
  lib,
  pkgs,
  config,
  osConfig,
  ...
}:
{
  gtk = {
    enable = true;
    catppuccin.enable = true;
    cursorTheme = config.stylix.cursor;
    iconTheme =
      let
        recolor-icons =
          pkgs.writers.writePython3 "recolor-icons" { libraries = [ pkgs.custom.color-manager ]; }
            ''
              import sys
              from color_manager import utils

              src = sys.argv[1]
              dest = sys.argv[2]
              name = "candy-icons"
              palettes = "${pkgs.custom.color-manager.src}/palettes/"
              palette = palettes + "catppuccin_${config.catppuccin.flavor}.json"

              utils.recolor(src, dest, name, palette)
            '';
      in
      {
        name = "candy-icons";
        # Merge Candy Icons and Sweet Folders into the same package and recolor
        package = pkgs.runCommand "recolored-icons" { } ''
          mkdir tmp
          cp --recursive --no-preserve=mode ${pkgs.candy-icons}/share/icons/candy-icons/* tmp
          cp --recursive --no-preserve=mode ${pkgs.sweet-folders}/share/icons/Sweet-Rainbow/Places/* tmp/places/48
          mkdir --parents $out/share/icons
          ${recolor-icons} tmp $out/share/icons
        '';
      };
  };

  qt = rec {
    enable = true;
    style.name = "kvantum";
    platformTheme = style;
  };

  home.packages = with pkgs; [
    # fallback icon theme
    adwaita-icon-theme
    # Tools for making catppuccin ports
    catppuccin-catwalk
    catppuccin-whiskers
    just
  ];

  # Define here instead of globally because there is no global accent option in ctp-nix
  stylix.cursor =
    let
      palette =
        (lib.importJSON "${config.catppuccin.sources.palette}/palette.json")
        .${config.catppuccin.flavor}.colors;
    in
    {
      name = "Breeze_Hacked";
      package = pkgs.breeze-hacked-cursor-theme.override {
        accentColor = "${palette.${config.catppuccin.accent}.hex}";
        baseColor = "${palette.base.hex}";
        borderColor = "${palette.base.hex}";
        logoColor = "${palette.text.hex}";
      };
    };

  # Inherit system-level settings
  catppuccin = osConfig.catppuccin;

  # Required for btop theme
  xdg.enable = true;
}
