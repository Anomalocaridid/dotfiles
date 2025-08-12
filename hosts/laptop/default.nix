{ lib, inputs, ... }:
let
  hostname = "laptop";
in
{
  flake = rec {
    nixosConfigurations.${hostname} = inputs.nixpkgs.lib.nixosSystem {
      modules = [
        { networking.hostName = hostname; }
        diskoConfigurations.${hostname}
        { config.facter.reportPath = ./facter.json; }
      ] ++ (lib.attrsets.attrValues inputs.self.modules.nixos);
    };
    diskoConfigurations.${hostname} = import ../_common/disko.nix {
      disk = "/dev/disk/by-id/nvme-WD_BLACK_SN770_500GB_23313J808877";
      memory = "16G";
    };
  };
}
