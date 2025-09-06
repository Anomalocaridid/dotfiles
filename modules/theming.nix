{ inputs, ... }:
{
  unify.modules.general = {
    nixos =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        imports = [ inputs.catppuccin.nixosModules.catppuccin ];

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
          sources.parsedPalette =
            (lib.importJSON "${config.catppuccin.sources.palette}/palette.json")
            .${config.catppuccin.flavor}.colors;
        };

        # Allow svg icons in various places
        programs.gdk-pixbuf.modulePackages = with pkgs; [ librsvg ];
      };
    home =
      {
        config,
        lib,
        pkgs,
        osConfig,
        ...
      }:
      {
        imports = [ inputs.catppuccin.homeModules.catppuccin ];

        gtk = {
          enable = true;
          theme =
            let
              size = "standard";
            in
            {
              name = "catppuccin-${config.catppuccin.flavor}-${config.catppuccin.accent}-${size}";
              package = pkgs.catppuccin-gtk.override {
                accents = [ config.catppuccin.accent ];
                inherit size;
                variant = config.catppuccin.flavor;
              };
            };
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
              palette = config.catppuccin.sources.parsedPalette;
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
            sources
            ;
          # Disable Catppuccin icons because I want to use a different theme
          gtk.icon.enable = false;
        };

        xdg = {
          # Required for btop theme
          enable = true;
          # Manually link gtk theme accents
          configFile =
            let
              gtk4Dir = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0";
            in
            {
              "gtk-4.0/assets".source = "${gtk4Dir}/assets";
              "gtk-4.0/gtk.css".source = "${gtk4Dir}/gtk.css";
              "gtk-4.0/gtk-dark.css".source = "${gtk4Dir}/gtk-dark.css";
            };
        };
      };
  };
}
