{ config, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  flake.modules = {
    # Persist Zoxide history
    nixos.general.environment.persistence.${persistDir}.users.${username}.directories = [
      ".local/share/zoxide"
    ];

    homeManager.general.programs.zoxide = {
      enable = true;
      # Alias as cd
      options = [ "--cmd cd" ];
    };
  };
}
