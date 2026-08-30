{ config, flake-parts-lib, ... }:
flake-parts-lib.importApply ../_common/host.nix {
  hostname = builtins.baseNameOf ./.;
  modules = with config.unify.modules; [
    appliance
    primaryUser
    kodi
  ];
  users = {
    ${config.flake.meta.username}.modules = with config.unify.modules; [ primaryUser ];
    kodi.modules = with config.unify.modules; [ kodi ];
  };
  diskoConfig = import ./_disko.nix {
    disk = "/dev/disk/by-id/nvme-SAMSUNG_MZVLV512HCJH-00000_S2J6NXAGB01619";
    memory = "8G";
  };
}
