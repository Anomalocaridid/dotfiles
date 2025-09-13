{ config, inputs, ... }:
{
  flake-file.inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    lix = {
      url = "https://git.lix.systems/lix-project/lix/archive/main.tar.gz";
      flake = false;
    };

    lix-module = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/main.tar.gz";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        lix.follows = "lix";
      };
    };

    # Use nix-index without having to generate the database locally
    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  unify.modules.general = {
    nixos =
      { lib, ... }:
      {
        imports = [ inputs.lix-module.nixosModules.default ];

        nix = {
          settings = {
            experimental-features = [
              "nix-command"
              "flakes"
              "pipe-operator" # Lix-specific feature
            ];
            auto-optimise-store = true;
            repl-overlays = [ ./_repl-overlay.nix ]; # Lix-specific setting
          };
          gc = {
            automatic = true;
            dates = "weekly";
            options = "--delete-older-than 14d";
          };

          # Set system registry to flake inputs
          # Remove non flake inputs, which cause errors
          # Flakes have an attribute _type, which equals "flake"
          # while non-flakes lack this attribute
          registry =
            inputs
            |> (lib.filterAttrs (_: flake: lib.attrsets.hasAttr "_type" flake))
            |> (lib.mapAttrs (_: flake: { inherit flake; }));
        };

        nixpkgs.config.allowUnfree = true;

        # Ensure that nixos config has proper permissions
        # NOTE: persistence permissions only seem to apply upon creating a bind mount
        systemd.tmpfiles.rules = [ "Z /etc/nixos - ${config.flake.meta.username} users -" ];

        # This value determines the NixOS release from which the default
        # settings for stateful data, like file locations and database versions
        # on your system were taken. It‘s perfectly fine and recommended to leave
        # this value at the release version of the first install of this system.
        # Before changing this value read the documentation for this option
        # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
        system.stateVersion = "22.11"; # Did you read the comment?
      };

    home =
      { config, osConfig, ... }:
      {
        imports = [ inputs.nix-index-database.homeModules.nix-index ];

        # Use nix-index to locate missing commands
        programs.nix-index.enable = true;

        # Link /etc/nixos to home directory
        systemd.user.tmpfiles.rules = [
          # Create a link to /etc/nixos, where the config is in the home directory
          "L ${config.home.homeDirectory}/nixos -   -            -     - /etc/nixos"
          # Ensure SSH keys have proper permissions.
          # NOTE: persistence permissions only seem to apply upon creating a bind mount
          # NOTE: Directory and contents need to have permissions set separately or else it gets set to root permissions for some reason
          "z ${config.home.homeDirectory}/.ssh 0700 ${config.home.username} users - -"
          "Z ${config.home.homeDirectory}/.ssh/* 0600 ${config.home.username} users - -"
        ];

        # DON'T TOUCH
        # Use system-level stateVersion
        home.stateVersion = osConfig.system.stateVersion;
      };
  };
}
