{ pkgs, inputs, ... }:
{
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    backupFileExtension = "bak";
    users.anomalocaris = {
      imports = [
        ../home-modules
        inputs.nix-index-database.hmModules.nix-index
        inputs.catppuccin.homeManagerModules.catppuccin
        inputs.spicetify-nix.homeManagerModules.spicetify
        inputs.nixcord.homeManagerModules.nixcord
        inputs.nix-yazi-plugins.legacyPackages.${pkgs.system}.homeManagerModules.default # WHY IS IT LIKE THIS!?
      ];
    };
    extraSpecialArgs = {
      inherit inputs;
    };
  };
}
