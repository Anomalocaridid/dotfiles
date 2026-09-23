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
        diskoConfigurations.${hostname}
        {
          networking.hostName = hostname;
          hardware.facter.reportPath = ../${hostname}/facter.json;
          home-manager = { inherit users; };
        }
      ];
    };

    diskoConfigurations.${hostname} = diskoConfig { inherit (config.flake.meta) persistDir; };
  };
}
