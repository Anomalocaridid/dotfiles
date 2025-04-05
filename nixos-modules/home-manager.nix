{ inputs, ... }:
{
  imports = [ inputs.home-manager.nixosModules.home-manager ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    backupFileExtension = "bak";
    users.anomalocaris = {
      imports = [
        ../home-modules
      ];
    };
    extraSpecialArgs = {
      inherit inputs;
    };
  };
}
