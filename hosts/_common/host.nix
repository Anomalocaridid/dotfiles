{
  hostname,
  modules,
  diskoConfig,
}:
{ config, inputs, ... }:
rec {
  unify.hosts.nixos.${hostname} = {
    inherit modules;
    nixos = {
      imports = [
        inputs.disko.nixosModules.disko
        inputs.nixos-facter-modules.nixosModules.facter
        flake.diskoConfigurations.${hostname}
      ];
      facter.reportPath = ../${hostname}/facter.json;
    };
    users.${config.flake.meta.username}.modules = config.unify.hosts.nixos.${hostname}.modules;
  };

  flake.diskoConfigurations.${hostname} = diskoConfig { inherit (config.flake.meta) persistDir; };
}
