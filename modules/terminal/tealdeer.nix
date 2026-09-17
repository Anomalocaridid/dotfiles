{ config, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  flake.modules = {
    # Tldr pages, prevents tealdeer redownloading them every time
    nixos.general.environment.persistence.${persistDir}.users.${username}.directories = [
      ".cache/tealdeer"
    ];

    homeManager.general = {
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
