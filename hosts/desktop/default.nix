{
  config,
  inputs,
  flake-parts-lib,
  ...
}:
flake-parts-lib.importApply ../_common/host.nix {
  hostname = builtins.baseNameOf ./.;

  nixosSystem = inputs.nixpkgs.lib.nixosSystem;

  modules = with config.flake.modules.nixos; [
    default
    general
    desktop
  ];

  users.${config.flake.meta.username}.imports = with config.flake.modules.homeManager; [
    default
    general
    desktop
    primaryUser
  ];

  diskoConfig = import ../_common/disko.nix {
    disk = "/dev/disk/by-id/nvme-WDS100T3X0C-00SJG0_20477T805943";
    memory = "32G";
  };
}
