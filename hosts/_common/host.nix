{
  hostname,
  nixosSystem,
  modules,
  users,
  diskoConfig,
}:
{ config, inputs, ... }:
{
  flake = rec {
    nixosConfigurations.${hostname} = nixosSystem {
      modules = modules ++ [
        inputs.disko.nixosModules.disko
        inputs.nixos-facter-modules.nixosModules.facter
        diskoConfigurations.${hostname}
        {
          networking.hostName = hostname;
          facter.reportPath = ../${hostname}/facter.json;
          home-manager = { inherit users; };
        }
      ];
    };

    diskoConfigurations.${hostname} = diskoConfig { inherit (config.flake.meta) persistDir; };
  };
}
