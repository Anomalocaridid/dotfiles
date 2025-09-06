{ config, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  unify.modules.general = {
    # Tldr pages, prevents tealdeer redownloading them every time
    nixos.environment.persistence.${persistDir}.users.${username}.directories = [ ".cache/tealdeer" ];

    home = {
      programs.tealdeer = {
        enable = true;
        settings = {
          display = {
            use_pager = true;
            compact = true;
          };
          updates.auto_update = true;
        };
      };

      home.shellAliases.tldr = "PAGER='bat --plain' command tldr";
    };
  };
}
