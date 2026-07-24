{ config, ... }:
let
  inherit (config.flake.meta) username persistDir;
in
{
  # Provides a binary cache, so do not follow inputs
  flake-file.inputs.nix-gaming.url = "github:fufexan/nix-gaming";

  unify.modules = {
    general = {
      nixos.environment.persistence.${persistDir}.users.${username}.directories = [
        ".PySolFC" # PySolFC settings and save data
      ];

      home =
        { pkgs, ... }:
        {
          home.packages = with pkgs; [ pysolfc ];
        };
    };

    desktop = {
      nixos =
        { pkgs, ... }:
        {
          # Nintendo Pro Controller / Joycon support
          services.joycond.enable = true;
          # Support Direct Rendering for 32-bit applications, like Wine
          hardware.graphics.enable32Bit = true;

          # nix-gaming cache
          nix.settings = {
            substituters = [ "https://nix-gaming.cachix.org" ];
            trusted-public-keys = [ "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4=" ];
          };

          environment.persistence.${persistDir}.users.${username}.directories = [
            ".config/itch" # Itch games and settings
            ".runelite" # Runelite settings and cache
          ];
        };

      home =
        { config, pkgs, ... }:
        {
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
