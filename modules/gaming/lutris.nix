{ config, ... }:
let
  inherit (config.flake.meta) username persistDir;
in
{
  unify.modules.desktop = {
    nixos = {
      # On-demand system optimization for gaming
      programs.gamemode.enable = true;

      environment.persistence.${persistDir}.users.${username}.directories = [
        ".cache/lutris" # Lutris banner cache
        ".config/lutris" # Lutris games and settings
        ".local/share/lutris" # Lutris runtime data
      ];
    };

    home = { osConfig, ... }: {
      programs.lutris = rec {
        enable = true;
        defaultWinePackage = builtins.head protonPackages;
        protonPackages = osConfig.programs.steam.extraCompatPackages;
      };
    };
  };
}
