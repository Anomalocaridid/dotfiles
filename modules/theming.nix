{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
{

  # TTY theming
  console = {
    catppuccin.enable = true;
    font =
      let
        font = config.stylix.fonts.monospace;
        sizes = config.stylix.fonts.sizes;
        mkttyfont = inputs.ttf-to-tty.packages.${pkgs.system}.mkttyfont;
        dpi = toString 80;
      in
      pkgs.runCommand "${font.package.name}.psf"
        { FONTCONFIG_FILE = pkgs.makeFontsConf { fontDirectories = [ font.package ]; }; }
        ''
          # Use fontconfig to select the correct .ttf or .otf file based on name
          # Command taken from stylix GRUB module
          font=$(
            ${lib.getExe' pkgs.fontconfig "fc-match"} \
            ${lib.escapeShellArg font.name} \
            --format=%{file}
          )
          cp $font .

          # Convert font from tty to psf
          ${lib.getExe mkttyfont} *.ttf ${toString sizes.terminal} ${dpi}
          cp *.psf $out
        '';
  };

  # Configure Qt theme
  qt = rec {
    enable = true;
    platformTheme = "gtk2";
    style = platformTheme;
  };

  # Configure GRUB theme
  boot.loader.grub = rec {
    catppuccin.enable = true;
    splashImage = lib.mkForce "${theme.content}/background.png";
    theme = lib.mkForce (
      pkgs.runCommand "catppuccin-grub-theme" { } ''
        mkdir -p "$out"
        cp -r ${config.catppuccin.sources.grub}/src/catppuccin-${config.catppuccin.flavor}-grub-theme/* "$out"/

        # Replace background
        rm "$out"/background.png
        cp ${pkgs.sources.catppuccin-fractal-wallpapers + "/05.png"} "$out"/background.png
      ''
    );
  };

  stylix = {
    # Remove when stylix#200 is resolved
    image = pkgs.sources.catppuccin-fractal-wallpapers + "/05.png";

    # Just use Stylix for fonts
    autoEnable = false;

    fonts = rec {
      sizes = {
        terminal = 11;
        popups = 12;
      };
      serif = monospace;
      sansSerif = monospace;
      monospace =
        let
          font = "FiraCode";
        in
        {
          package = pkgs.nerdfonts.override { fonts = [ font ]; };
          name = "${font} Nerd Font";
        };
    };
  };

  catppuccin = {
    flavor = "mocha";
    accent = "mauve";
  };
}
