{
  config,
  inputs,
  flake-parts-lib,
  ...
}:
flake-parts-lib.importApply ../_common/host.nix {
  hostname = builtins.baseNameOf ./.;

  nixosSystem = inputs.nixos-raspberrypi.lib.nixosSystem;

  modules = with config.flake.modules.nixos; [
    default
    appliance
    kodi
    {
      imports = with inputs.nixos-raspberrypi.nixosModules.raspberry-pi-5; [
        base
        display-vc4
      ];

      boot.loader.raspberry-pi.bootloader = "kernel";
    }
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
