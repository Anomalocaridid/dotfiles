{ inputs, ... }:
{
  # Provides a binary cache, so do not follow inputs
  flake-file.inputs.catppuccin.url = "github:catppuccin/nix";

  perSystem =
    { pkgs, ... }:
    {
      # Tools for making catppuccin ports
      devshells.catppuccin.packages = with pkgs; [
        catppuccin-catwalk
        catppuccin-whiskers
      ];
    };

  unify.modules.general = {
    nixos =
      { config, lib, ... }:
      {
        imports = [ inputs.catppuccin.nixosModules.catppuccin ];

        catppuccin = {
          enable = true;
          cache.enable = true;
          flavor = "mocha";
          accent = "mauve";
          sources.parsedPalette =
            (lib.importJSON "${config.catppuccin.sources.palette}/palette.json")
            .${config.catppuccin.flavor}.colors;
        };
      };
    home =
      {
        config,
        pkgs,
        osConfig,
        ...
      }:
      {
        imports = [ inputs.catppuccin.homeModules.catppuccin ];

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

        # Manually set up catppuccin GTK theme since it is no longer supported upstream
        gtk = {
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
        };

        # Manually link GTK theme accents
        xdg.configFile =
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
}
