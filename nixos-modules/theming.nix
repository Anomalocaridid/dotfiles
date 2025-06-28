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
  ];

  # TTY theming
  console = {
    font =
      let
        font_family = builtins.head config.fonts.fontconfig.defaultFonts.monospace;
        font_size = toString 11;
        font_pkg = builtins.head config.fonts.packages;
        mkttyfont = inputs.ttf-to-tty.packages.${pkgs.system}.mkttyfont;
        dpi = toString 80;
      in
      pkgs.runCommand "${font_family}.psf"
        { FONTCONFIG_FILE = pkgs.makeFontsConf { fontDirectories = [ font_pkg ]; }; }
        ''
          # Use fontconfig to select the correct .ttf or .otf file based on name
          # Command taken from stylix GRUB module
          font=$(
            ${lib.getExe' pkgs.fontconfig "fc-match"} \
            ${lib.escapeShellArg font_family} \
            --format=%{file}
          )
          cp $font .

          # Convert font from tty to psf
          ${lib.getExe mkttyfont} *.ttf ${font_size} ${dpi}
          cp *.psf $out
        '';
  };

  # Configure GRUB theme
  boot.loader.grub =
    let
      wallpaper = inputs.catppuccin-fractal-wallpapers + "/05.png";
    in
    {
      splashImage = lib.mkForce wallpaper;
      theme = lib.mkForce (
        pkgs.runCommand "catppuccin-grub-background" { } ''
          cp --recursive --no-preserve=mode ${config.catppuccin.sources.grub}/share/grub/themes/catppuccin-${config.catppuccin.flavor}-grub-theme/ "$out"

          # Replace background
          rm "$out"/background.png
          cp ${wallpaper} "$out"/background.png
        ''
      );
    };

  fonts = {
    packages = with pkgs; [ nerd-fonts.fira-code ];

    fontconfig = {
      defaultFonts = {
        serif = [ "FiraCode Nerd Font Propo" ];
        sansSerif = [ "FiraCode Nerd Font Propo" ];
        monospace = [ "FiraCode Nerd Font" ];
      };
      localConf = # xml
        ''
          <match target="font">
            <test name="family" compare="contains">
              <string>FiraCode Nerd Font</string>
            </test>
            <edit name="fontfeatures" mode="append">
              <string>ss09</string> <!-- >>= <<= ||= |= -->
              <string>cv25</string> <!-- .- -->
              <string>cv26</string> <!-- :- -->
              <string>cv32</string> <!-- .= -->
              <string>cv27</string> <!-- [] -->
              <string>cv28</string> <!-- {. .} -->
              <string>ss06</string> <!-- \\ -->
              <string>ss07</string> <!-- =~ !~ -->
            </edit>
          </match>
        '';
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
