{
  description = "My personal dotfiles for NixOS";
  
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    impermanence.url = "github:nix-community/impermanence";
    # impermanence.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {self, nixpkgs, disko, home-manager, impermanence}: {
    nixosConfigurations.home-pc = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      
      modules = [
        ./configuration.nix
        disko.nixosModules.disko
        home-manager.nixosModules.home-manager
        impermanence.nixosModules.impermanence
        # impermanence.nixosModules.home-manager.impermanence
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.anomalocaris = import ./home.nix;
          environment.persistence = import ./persistance.nix;
        }
      ];
    };
  };
}
