{ config, inputs, ... }:
let
  inherit (config.flake.meta) username persistDir;
in
{
  flake.meta = rec {
    persistDir = "/persist";
    passwordDir = "${persistDir}/passwords";
  };

  flake-file.inputs.impermanence = {
    url = "github:nix-community/impermanence";
    inputs = {
      nixpkgs.follows = "nixpkgs";
      home-manager.follows = "home-manager";
    };
  };

  unify = {
    nixos =
      { config, ... }:
      {
        imports = [ inputs.impermanence.nixosModules.impermanence ];

        # Ensure that nixos config has proper permissions
        # NOTE: persistence permissions only seem to apply upon creating a bind mount
        systemd.tmpfiles.rules = [ "Z /etc/nixos - ${username} ${config.users.users.${username}.group} -" ];

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
            # SSH key
            {
              directory = ".ssh";
              mode = "u=rwx,g=,o=";
            }
          ];
        };
      };

    home = { config, osConfig, ... }: {
      systemd.user.tmpfiles.rules = [
        # Create a link to /etc/nixos, where the config is, in the home directory
        "L ${config.home.homeDirectory}/nixos -   -            -     - /etc/nixos"
        # Ensure SSH keys have proper permissions.
        # NOTE: persistence permissions only seem to apply upon creating a bind mount
        # NOTE: Directory and contents need to have permissions set separately or else it gets set to root permissions for some reason
        "z ${config.home.homeDirectory}/.ssh 0700 ${config.home.username} ${
          osConfig.users.users.${config.home.username}.group
        } - -"
        "Z ${config.home.homeDirectory}/.ssh/* 0600 ${config.home.username} ${
          osConfig.users.users.${config.home.username}.group
        } - -"
      ];
    };

    modules.general.nixos.environment.persistence.${persistDir} = {
      users.${username}.directories = [
        # Default directories I care about
        "Documents"
        "Downloads"
        "Games"
        "Music"
        "Pictures"
        "Projects"
        "Videos"
      ];
    };
  };
}
