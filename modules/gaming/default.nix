{ config, ... }:
let
  inherit (config.flake.meta) username persistDir;

  # nix-gaming cache
  cacheSettings = {
    extra-substituters = [ "https://nix-gaming.cachix.org" ];
    extra-trusted-public-keys = [
      "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4="
    ];
  };
in
{
  flake-file = {
    # Provides a binary cache, so do not follow inputs
    inputs.nix-gaming.url = "github:fufexan/nix-gaming";
    nixConfig = cacheSettings;
  };

  flake.modules = {
    nixos = {
      general.environment.persistence.${persistDir}.users.${username}.directories = [
        ".PySolFC" # PySolFC settings and save data
      ];

      desktop = { pkgs, ... }: {
        # Nintendo Pro Controller / Joycon support
        services.joycond.enable = true;
        # Support Direct Rendering for 32-bit applications, like Wine
        hardware.graphics.enable32Bit = true;
        # Set nix-gaming cache
        nix.settings = cacheSettings;

        environment.persistence.${persistDir}.users.${username}.directories = [
          ".config/itch" # Itch games and settings
          ".runelite" # Runelite settings and cache
        ];
      };
    };

    homeManager = {
      general = { pkgs, ... }: {
        home.packages = with pkgs; [ pysolfc ];
      };

      desktop = { config, pkgs, ... }: {
        home = {
          packages = with pkgs; [
            itch
            runelite
          ];

          # Enable wine-ge's fsync support
          sessionVariables.WINEFSYNC = 1;
        };
      };
    };
  };
}
