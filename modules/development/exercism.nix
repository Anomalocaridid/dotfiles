{ config, ... }:
let
  inherit (config.flake.meta) username persistDir;
in
{
  flake.modules = {
    nixos.general.environment.persistence.${persistDir}.users.${username}.directories = [
      "exercism" # Exercism exercises
      ".config/exercism" # Exercism API key
    ];

    homeManager.general = { pkgs, ... }: {
      home.packages = with pkgs; [ exercism ];
    };
  };
}
