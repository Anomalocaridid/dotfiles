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

    advcpmv = {
      url = "github:jarun/advcpmv";
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
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.anomalocaris = {
            imports = [
              inputs.impermanence.nixosModules.home-manager.impermanence
              ./home.nix
            ];
          };

          # Impermanence
          environment.persistence = import ./persistance.nix;
        }
      ];
      # Inherit inputs to use advcpmv patch
      specialArgs = {
        inherit inputs;
      };
    };
    # Expose this to use flake directly with Disko
    diskoConfigurations.home-pc = import ./disko-config.nix;
  };
}
