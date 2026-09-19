{ inputs, ... }:
{
  flake-file = {
    # Provides a binary cache, so do not follow inputs
    inputs.catppuccin.url = "github:catppuccin/nix";

    nixConfig = {
      extra-substituters = [ "https://catppuccin.cachix.org" ];
      extra-trusted-public-keys = [
        "catppuccin.cachix.org-1:noG/4HkbhJb+lUAdKrph6LaozJvAeEEZj4N732IysmU="
      ];
    };
  };

  perSystem = { pkgs, ... }: {
    # Tools for making catppuccin ports
    devshells.catppuccin.packages = with pkgs; [
      catppuccin-catwalk
      catppuccin-whiskers
    ];
  };

  flake.modules = {
    nixos.default = { config, lib, ... }: {
      imports = [ inputs.catppuccin.nixosModules.catppuccin ];

      catppuccin = {
        enable = true;
        autoEnable = true;
        cache.enable = true;
        flavor = "mocha";
        accent = "mauve";
        sources.parsedPalette =
          (lib.importJSON "${config.catppuccin.sources.palette}/palette.json")
          .${config.catppuccin.flavor}.colors;
      };
    };

    homeManager.default = { osConfig, ... }: {
      imports = [ inputs.catppuccin.homeModules.catppuccin ];

      # Inherit system-level settings
      # Do not inherit cache setting or else the other system-level caches will not be used
      catppuccin = {
        inherit (osConfig.catppuccin)
          enable
          autoEnable
          flavor
          accent
          sources
          ;
      };
    };
  };
}
