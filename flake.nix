{
  description = "My personal dotfiles for NixOS";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    impermanence.url = "github:nix-community/impermanence";

    # Use nix-index without having to generate the database locally
    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Nix User Repo
    nur.url = "github:nix-community/NUR";

    # Up to date Unison packages
    unison-nix = {
      url = "github:ceedubs/unison-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Patch for progress bars for cp and mv
    advcpmv = {
      url = "github:jarun/advcpmv";
      flake = false;
    };

    # Fork of zsh-bd zsh plugin
    zsh-bd = {
      url = "github:mawkler/zsh-bd";
      flake = false;
    };

    # Hooks to simplify zle widget code
    zsh-hooks = {
      url = "github:zsh-hooks/zsh-hooks";
      flake = false;
    };

    # Nyxt plugins
    nx-freestance-handler = {
      url = "github:kssytsrk/nx-freestance-handler";
      flake = false;
    };

    nx-fruit = {
      url = "github:atlas-engineer/nx-fruit";
      flake = false;
    };

    nx-kaomoji = {
      url = "github:aartaka/nx-kaomoji";
      flake = false;
    };

    nx-search-engines = {
      url = "github:aartaka/nx-search-engines";
      flake = false;
    };
  };

  outputs = inputs: {
    nixosConfigurations.home-pc = inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";

      modules = [
        ./configuration.nix
        inputs.disko.nixosModules.disko
        (import ./disko-config.nix {
          disk = "/dev/vda";
          memory = "8G";
        })
        inputs.nur.nixosModules.nur
        inputs.home-manager.nixosModules.home-manager
        inputs.impermanence.nixosModules.impermanence
        {
          # Home Manager
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            users.anomalocaris = {
              imports = [
                ./home.nix
                inputs.impermanence.nixosModules.home-manager.impermanence
                inputs.nix-index-database.hmModules.nix-index
              ];
            };
            # Inherit inputs to use zsh plugins not in nixpkgs
            extraSpecialArgs = {
              inherit inputs;
            };
          };

          # Impermanence
          environment.persistence = import
            ./persistence.nix;

          nixpkgs.overlays = [
            # cp and mv with progress bars
            (final: prev: {
              advcpmv-coreutils = prev.coreutils.overrideAttrs (oldAttrs: {
                patches = (oldAttrs.patches or [ ]) ++ [ "${inputs.advcpmv}/advcpmv-0.9-${oldAttrs.version}.patch" ];
              });
            })
            # Up to date Unison packages
            inputs.unison-nix.overlay
          ];
        }
      ];
    };
    # Expose this to use flake directly with Disko
    diskoConfigurations.home-pc = import
      ./disko-config.nix;
  };
}
