{
  config,
  lib,
  pkgs,
  osConfig,
  inputs,
  ...
}:
{

  imports = [ inputs.catppuccin.homeModules.catppuccin ];

  # Does not use global enable option for some reason
  catppuccin.gtk.enable = true;

  gtk = {
    enable = true;
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

  fonts.fontconfig.defaultFonts = osConfig.fonts.fontconfig.defaultFonts;

  home = {
    pointerCursor =
      let
        palette =
          (lib.importJSON "${config.catppuccin.sources.palette}/palette.json")
          .${config.catppuccin.flavor}.colors;
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
    packages = with pkgs; [
      # fallback icon theme
      adwaita-icon-theme
      # Tools for making catppuccin ports
      catppuccin-catwalk
      catppuccin-whiskers
      just
    ];
  };

  # Inherit system-level settings
  catppuccin = {
    inherit (osConfig.catppuccin)
      enable
      cache
      flavor
      accent
      ;
  };

  # Required for btop theme
  xdg.enable = true;
}
