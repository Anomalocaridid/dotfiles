{ inputs, ... }:
{
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.anomalocaris = {
      imports = [
        ../home
        inputs.impermanence.nixosModules.home-manager.impermanence
        inputs.nix-index-database.hmModules.nix-index
        inputs.hyprlock.homeManagerModules.hyprlock
        inputs.hypridle.homeManagerModules.hypridle
        inputs.catppuccin.homeManagerModules.catppuccin
        inputs.ssbm-nix.homeManagerModule
        inputs.spicetify-nix.homeManagerModules.spicetify
      ];
    };
    extraSpecialArgs = {
      inherit inputs;
    };
  };
}
