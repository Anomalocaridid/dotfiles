{
  lib,
  pkgs,
  config,
  osConfig,
  inputs,
  ...
}:
{
  gtk = {
    enable = true;
    catppuccin.enable = true;
    cursorTheme = config.stylix.cursor;
    iconTheme = {
      name = "candy-icons";
      # Merge Candy Icons and Sweet Folders into the same package
      package = pkgs.candy-icons.overrideAttrs (oldAttrs: {
        postInstall = ''
          cp ${pkgs.sweet-folders}/share/icons/Sweet-Rainbow/Places/* $out/share/icons/candy-icons/places/48
        '';
      });
    };
  };

  qt = rec {
    enable = true;
    style.name = "kvantum";
    platformTheme = style;
  };

  home = {
    packages = with pkgs; [
      # fallback icon theme
      adwaita-icon-theme
      # Tools for making catppuccin ports
      inputs.catppuccin-catwalk.packages.${pkgs.system}.catwalk
      inputs.catppuccin-whiskers.packages.${pkgs.system}.whiskers
      just
    ];
  };

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
