{ config, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  unify.modules.general = {
    nixos.environment.persistence.${persistDir}.users.${username}.directories = [
      ".local/share/zoxide" # Zoxide history
    ];

    home.programs.zoxide = {
      enable = true;
      # Alias as cd
      options = [ "--cmd cd" ];
    };
  };
}
