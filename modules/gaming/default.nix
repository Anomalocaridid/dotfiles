{ config, ... }:
{
  # Provides a binary cache, so do not follow inputs
  flake-file.inputs.nix-gaming.url = "github:fufexan/nix-gaming";

  unify.modules.general = {
    nixos =
      { pkgs, ... }:
      let
        inherit (config.flake.meta) username persistDir;
      in
      {
        # On-demand system optimization for gaming
        programs.gamemode.enable = true;

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
          ".cache/lutris" # Lutris banner cache
          ".config/lutris" # Lutris games and settings
          ".local/share/lutris" # Lutris runtime data
          ".PySolFC" # PySolFC settings and save data
          ".runelite" # Runelite settings and cache
        ];
      };

    home =
      { config, pkgs, ... }:
      {
        home.packages = with pkgs; [
          gamescope # Used by Lutris for control over game resolution
          lutris
          pysolfc
          runelite
        ];

        # Enable wine-ge's fsync support
        home.sessionVariables.WINEFSYNC = 1;
      };
  };
}
