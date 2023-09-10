{ inputs, ... }: {
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.anomalocaris = {
      imports = [
        ../home
        inputs.impermanence.nixosModules.home-manager.impermanence
        inputs.nix-index-database.hmModules.nix-index
        inputs.ssbm-nix.homeManagerModule
      ];
    };
    extraSpecialArgs = {
      inherit inputs;
    };
  };
}
