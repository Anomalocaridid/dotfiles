{ config, inputs, ... }:
let
  inherit (config.flake.meta) username persistDir;
in
{
  flake.meta = rec {
    persistDir = "/persist";
    passwordDir = "${persistDir}/passwords";
  };

  flake-file.inputs.impermanence.url = "github:nix-community/impermanence";

  unify.modules.general.nixos =
    { config, ... }:
    {
      imports = [ inputs.impermanence.nixosModules.impermanence ];

      environment.persistence.${persistDir} = {
        hideMounts = true;
        directories = [
          # Necessary system state
          ## NixOS
          "/var/lib/nixos" # Holds state needed for stable uids and gids for users and groups
          ## systemd
          "/var/lib/systemd" # Systemd state directory, used for numerous things
          # Nix config
          {
            directory = "/etc/nixos";
            user = username;
            group = config.users.users.${username}.group;
          }
        ];
        files = [
          # Necessary system state
          ## systemd
          "/etc/machine-id" # Unique system id for logging, etc.
        ];
        users.${username}.directories = [
          # Default directories I care about
          "Documents"
          "Downloads"
          "Games"
          "Music"
          "Pictures"
          "Videos"

          # Other important stuff
          "Projects" # Misc. programming
          # SSH key
          {
            directory = ".ssh";
            mode = "u=rwx,g=,o=";
          }
        ];
      };
    };
}
