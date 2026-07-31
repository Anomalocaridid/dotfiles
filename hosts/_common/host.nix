{
  hostname,
  modules,
  users,
  diskoConfig,
}:
{ config, inputs, ... }:
rec {
  unify.hosts.nixos.${hostname} = {
    inherit modules users;
    nixos = {
      imports = [
        inputs.disko.nixosModules.disko
        inputs.nixos-facter-modules.nixosModules.facter
        flake.diskoConfigurations.${hostname}
      ];
      facter.reportPath = ../${hostname}/facter.json;
    };
  };

  flake.diskoConfigurations.${hostname} = diskoConfig { inherit (config.flake.meta) persistDir; };
}
