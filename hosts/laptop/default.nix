{ config, lib, ... }:
let
  hostname = "laptop";
in
rec {
  unify.hosts.nixos.${hostname} = {
    modules = lib.attrsets.attrValues config.unify.modules;
    nixos = {
      networking.hostName = hostname;
      imports = [ flake.diskoConfigurations.${hostname} ];
      facter.reportPath = ./facter.json;
    };
    users.anomalocaris.modules = config.unify.hosts.nixos.${hostname}.modules;
  };

  flake.diskoConfigurations.${hostname} = import ../_common/disko.nix {
    disk = "/dev/disk/by-id/nvme-WD_BLACK_SN770_500GB_23313J808877";
    memory = "16G";
  };
}
