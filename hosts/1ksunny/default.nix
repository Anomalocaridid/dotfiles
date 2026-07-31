{ config, flake-parts-lib, ... }:
flake-parts-lib.importApply ../_common/host.nix rec {
  hostname = builtins.baseNameOf ./.;
  modules = with config.unify.modules; [
    appliance
    primaryUser
  ];
  users.${config.flake.meta.username}.modules = config.unify.hosts.nixos.${hostname}.modules;
  diskoConfig = import ./_disko.nix {
    disk = "/dev/disk/by-id/nvme-SAMSUNG_MZVLV512HCJH-00000_S2J6NXAGB01619";
    memory = "8G";
  };
}
