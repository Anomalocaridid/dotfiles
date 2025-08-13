{ inputs, ... }:
{
  unify.modules.home-manager = {
    nixos =
      { lib, ... }:
      {
        imports = [ inputs.home-manager.nixosModules.home-manager ];

        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          backupFileExtension = "bak";
        };
      };
    # Lets Home Manager manage itself
    home.programs.home-manager.enable = true;
  };
}
