{ config, flake-parts-lib, ... }:
flake-parts-lib.importApply ../_common/host.nix {
  hostname = builtins.baseNameOf ./.;
  modules = with config.unify.modules; [
    htpc
  ];
  diskoConfig = import ./_disko.nix {
    # TODO: replace with actual disk
    disk = "/dev/vda";
  };
}
