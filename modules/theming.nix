{ config, pkgs, inputs, ... }: {

  # TTY theming
  console = {
    font =
      let
        font = config.stylix.fonts.monospace;
        size = toString config.stylix.fonts.sizes.terminal;
        mkttyfont = inputs.ttf-to-tty.packages.${pkgs.system}.mkttyfont;
        dpi = toString 80;
      in
      pkgs.runCommand "${font.name}-${size}.psf"
        {
          FONTCONFIG_FILE = pkgs.makeFontsConf { fontDirectories = [ font.package ]; };
        } ''
        export XDG_CACHE_HOME="$(mktemp -d)"
        # Use fontconfig to select the correct .ttf or .otf file based on name
        # Command taken from Stylix GRUB module
        fontPath=$(${pkgs.fontconfig}/bin/fc-match -v "${font.name}" | grep "file:" | cut -d '"' -f 2)
        cp $fontPath .
        
        # Convert font from tty to psf
        ${mkttyfont}/bin/mkttyfont *.ttf ${size} ${dpi}
        cp *.psf $out
      '';
    colors =
      let
        palette = pkgs.custom.catppuccin-palette.${config.catppuccin.flavour};
      in
      [
        # Normal
        palette.base.hex
        palette.red.hex
        palette.green.hex
        palette.yellow.hex
        palette.blue.hex
        palette.pink.hex
        palette.teal.hex
        palette.subtext1.hex

        # Bright
        palette.surface2.hex
        palette.red.hex
        palette.green.hex
        palette.yellow.hex
        palette.blue.hex
        palette.pink.hex
        palette.teal.hex
        palette.subtext0.hex
      ];
  };

  # Configure Qt theme
  qt = rec {
    enable = true;
    platformTheme = "gtk2";
    style = platformTheme;
  };

  stylix = {
    image = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/zhichaoh/catppuccin-wallpapers/main/mandelbrot/mandelbrot_gap_magenta.png";
      hash = "sha256-mDawcbfc7uegqq2CHVT/MXOnVnSh/8xnqBgaXssWBp4=";
    };

    base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-mocha.yaml";
    # Just use Stylix for GRUB theme
    autoEnable = false;
    targets.grub = {
      enable = true;
      useImage = true;
    };

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

  catppuccin.flavour = "mocha";
}
