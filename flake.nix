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
            # Inherit inputs to use advcpmv patch
            extraSpecialArgs = {
              inherit inputs;
            };
          };

          # Impermanence
          environment.persistence = import
            ./persistance.nix;
        }
      ];
    };
    # Expose this to use flake directly with Disko
    diskoConfigurations.home-pc = import ./disko-config.nix;
  };
}
