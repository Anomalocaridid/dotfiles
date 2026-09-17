{ inputs, ... }:
{
  flake-file = {
    inputs = {
      nixos-hardware.url = "github:NixOS/nixos-hardware/master";
      # Has a cache, so do not override inputs
      nixos-raspberrypi.url = "github:nvmd/nixos-raspberrypi/develop";
    };

    # nixos-raspberrypi cache flake hint
    nixConfig = {
      extra-substituters = [
        "https://nixos-raspberrypi.cachix.org"
      ];
      extra-trusted-public-keys = [
        "nixos-raspberrypi.cachix.org-1:4iMO9LXa8BqhU+Rpg6LQKiGa2lsNh/j2oiYLNOQ5sPI="
      ];
    };
  };

  flake.modules.nixos.laptop.imports = [ inputs.nixos-hardware.nixosModules.framework-16-7040-amd ];
}
