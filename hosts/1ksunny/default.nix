{ config, flake-parts-lib, ... }:
flake-parts-lib.importApply ../_common/host.nix rec {
  hostname = builtins.baseNameOf ./.;
  modules = with config.unify.modules; [
    appliance
    primaryUser
  ];
  users.${config.flake.meta.username}.modules = config.unify.hosts.nixos.${hostname}.modules;
  diskoConfig = import ./_disko.nix {
    # TODO: replace with actual disk
    disk = "/dev/vda";
  };
}
