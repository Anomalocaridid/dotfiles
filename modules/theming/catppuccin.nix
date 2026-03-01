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
      { osConfig, ... }:
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
        };
      };
  };
}
