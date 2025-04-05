{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
{

  imports = [
    inputs.catppuccin.nixosModules.catppuccin
    inputs.stylix.nixosModules.stylix
  ];

  # TTY theming
  console = {
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

  # Configure GRUB theme
  boot.loader.grub = {
    splashImage = lib.mkForce config.stylix.image;
    theme = lib.mkForce (
      pkgs.runCommand "catppuccin-grub-background" { } ''
        cp --recursive --no-preserve=mode ${config.catppuccin.sources.grub}/share/grub/themes/catppuccin-${config.catppuccin.flavor}-grub-theme/ "$out"

        # Replace background
        rm "$out"/background.png
        cp ${config.stylix.image} "$out"/background.png
      ''
    );
  };

  stylix = {
    enable = true;
    image = inputs.catppuccin-fractal-wallpapers + "/05.png";

    # Set a theme just so one does not have to be automatically generated
    # Remove when stylix#248 is resolved.
    base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-mocha.yaml";

    # Just use Stylix for fonts
    autoEnable = false;

    fonts =
      let
        package = pkgs.nerd-fonts.fira-code;
        name = "FiraCode Nerd Font";
      in
      rec {
        sizes = {
          terminal = 11;
          popups = 12;
        };
        serif = sansSerif;
        sansSerif = {
          inherit package;
          name = "${name} Propo";
        };
        monospace = {
          inherit package name;
        };
      };

    cursor =
      let
        palette =
          (lib.importJSON "${config.catppuccin.sources.palette}/palette.json")
          .${config.catppuccin.flavor}.colors;
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

  catppuccin = {
    enable = true;
    cache.enable = true;
    flavor = "mocha";
    accent = "mauve";
  };

  # Allow svg icons in various places
  programs.gdk-pixbuf.modulePackages = with pkgs; [ librsvg ];
}
