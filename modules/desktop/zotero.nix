{ config, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  unify.modules.general = {
    nixos.environment.persistence.${persistDir}.users.${username}.directories = [
      "Zotero" # Zotero databases
      ".zotero" # Zotero settings and extensions
    ];

    home =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [ zotero ];

        programs.librewolf.policies.ExtensionSettings."zotero@chnm.gmu.edu" = {
          install_url = "https://www.zotero.org/download/connector/dl?browser=firefox";
          installation_mode = "force_installed";
        };
      };
  };
}
