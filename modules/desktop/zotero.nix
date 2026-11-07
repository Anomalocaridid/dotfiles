{ config, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  flake.modules = {
    nixos.general = {
      # Zotero relies on an unfree, insecure version of Firefox
      nixpkgs.config = {
        allowUnfreePackages = [ "firefox-esr-140-bin-unwrapped" ];
        permittedInsecurePackages = [ "firefox-esr-140-bin-unwrapped-140.17.0esr" ];
      };

      environment.persistence.${persistDir}.users.${username}.directories = [
        "Zotero" # Zotero databases
        ".zotero" # Zotero settings and extensions
      ];
    };

    homeManager.general = { pkgs, ... }: {
      home.packages = with pkgs; [ zotero ];

      programs.librewolf.policies.ExtensionSettings."zotero@chnm.gmu.edu" = {
        install_url = "https://www.zotero.org/download/connector/dl?browser=firefox";
        installation_mode = "force_installed";
      };
    };
  };
}
