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

  };

  outputs = inputs: {
      nixosConfigurations.home-pc = inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        modules = [
          ./configuration.nix
          inputs.disko.nixosModules.disko
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
        # Inherit inputs to use extra base16 themes with Stylix
        specialArgs = {
          inherit inputs;
        };
      };
    };
}
