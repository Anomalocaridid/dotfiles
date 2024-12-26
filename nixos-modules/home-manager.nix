{ inputs, ... }:
{
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.anomalocaris = {
      imports = [
        ../home-modules
        inputs.nix-index-database.hmModules.nix-index
        inputs.catppuccin.homeManagerModules.catppuccin
        inputs.spicetify-nix.homeManagerModules.spicetify
        inputs.nixcord.homeManagerModules.nixcord
      ];
    };
    extraSpecialArgs = {
      inherit inputs;
    };
  };
}
