{ inputs, ... }:
{
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.anomalocaris = {
      imports = [
        ../home-modules
        inputs.impermanence.nixosModules.home-manager.impermanence
        inputs.nix-index-database.hmModules.nix-index
        inputs.catppuccin.homeManagerModules.catppuccin
        inputs.spicetify-nix.homeManagerModules.spicetify
      ];
    };
    extraSpecialArgs = {
      inherit inputs;
    };
  };
}
