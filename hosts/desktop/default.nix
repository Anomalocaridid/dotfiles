{ config, lib, ... }:
let
  hostname = "desktop";
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
    disk = "/dev/disk/by-id/nvme-WDS100T3X0C-00SJG0_20477T805943";
    memory = "32G";
  };
}
