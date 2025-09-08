{ config, flake-parts-lib, ... }:
flake-parts-lib.importApply ../_common/host.nix {
  hostname = builtins.baseNameOf ./.;
  modules = with config.unify.modules; [ general ];
  diskoConfig = import ../_common/disko.nix {
    disk = "/dev/disk/by-id/nvme-WD_BLACK_SN770_500GB_23313J808877";
    memory = "16G";
  };
}
