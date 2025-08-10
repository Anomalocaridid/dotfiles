{ lib, inputs, ... }:
let
  hostname = "desktop";
in
{
  flake = rec {
    nixosConfigurations.${hostname} = inputs.nixpkgs.lib.nixosSystem {
      modules =
        [
          # Include the results of the hardware scan.
          ./configuration.nix
          { networking.hostName = hostname; }
          diskoConfigurations.${hostname}
        ]
        ++
        # TODO: remove and find a better way to do this (Unify?)
        (lib.attrsets.attrValues inputs.self.modules.nixos);
    };
    diskoConfigurations.${hostname} = import ../_common/disko.nix {
      disk = "/dev/disk/by-id/nvme-WDS100T3X0C-00SJG0_20477T805943";
      memory = "32G";
    };
  };
}
