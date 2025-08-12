{ lib, inputs, ... }:
let
  hostname = "desktop";
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
      disk = "/dev/disk/by-id/nvme-WDS100T3X0C-00SJG0_20477T805943";
      memory = "32G";
    };
  };
}
