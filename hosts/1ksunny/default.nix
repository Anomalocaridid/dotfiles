{ config, flake-parts-lib, ... }:
flake-parts-lib.importApply ../_common/host.nix {
  hostname = builtins.baseNameOf ./.;

  modules = with config.flake.modules.nixos; [
    default
    appliance
    kodi
  ];

  users = {
    ${config.flake.meta.username}.imports = with config.flake.modules.homeManager; [
      default
      primaryUser
    ];

    kodi.imports = with config.flake.modules.homeManager; [
      default
      kodi
    ];
  };

  diskoConfig = import ./_disko.nix {
    disk = "/dev/disk/by-id/nvme-SAMSUNG_MZVLV512HCJH-00000_S2J6NXAGB01619";
    memory = "8G";
  };
}
