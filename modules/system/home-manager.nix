{ inputs, ... }:
{
  flake.modules = {
    nixos.home-manager =
      { lib, ... }:
      {
        imports = [ inputs.home-manager.nixosModules.home-manager ];

        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          backupFileExtension = "bak";
          users.anomalocaris.imports = lib.attrsets.attrValues inputs.self.modules.homeManager;
        };
      };
    # Lets Home Manager manage itself
    homeManager.home-manager.programs.home-manager.enable = true;
  };
}
