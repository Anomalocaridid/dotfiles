{ lib, inputs, ... }:
let
  hostname = "laptop";
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
      disk = "/dev/disk/by-id/nvme-WD_BLACK_SN770_500GB_23313J808877";
      memory = "16G";
    };
  };
}
