{ config, flake-parts-lib, ... }:
flake-parts-lib.importApply ../_common/host.nix {
  hostname = builtins.baseNameOf ./.;
  modules = with config.unify.modules; [ general ];
  diskoConfig = import ../_common/disko.nix {
    disk = "/dev/disk/by-id/nvme-WDS100T3X0C-00SJG0_20477T805943";
    memory = "32G";
  };
}
