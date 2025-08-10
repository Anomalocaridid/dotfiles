{ inputs, ... }:
{
  flake.modules = {
    nixos.theming =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {

        imports = [ inputs.catppuccin.nixosModules.catppuccin ];

        # TTY theming
        console.font =
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
      };
    homeManager.theming =
      {
        config,
        lib,
        pkgs,
        osConfig,
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
        # Do not inherit cache setting or else the other system-level caches will not be used
        catppuccin = {
          inherit (osConfig.catppuccin)
            enable
            flavor
            accent
            ;
        };

        # Required for btop theme
        xdg.enable = true;
      };
  };
}
