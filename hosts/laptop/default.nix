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
    laptop
  ];

  users.${config.flake.meta.username}.imports = with config.flake.modules.homeManager; [
    default
    general
    laptop
    primaryUser
  ];

  diskoConfig = import ../_common/disko.nix {
    disk = "/dev/disk/by-id/nvme-WD_BLACK_SN770_500GB_23313J808877";
    memory = "16G";
  };
}
