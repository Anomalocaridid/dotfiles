{
  hostname,
  modules,
  diskoConfig,
  facterReportPath,
}:
{ config, lib, ... }:
rec {
  unify.hosts.nixos.${hostname} = {
    inherit modules;
    nixos = {
      imports = [ flake.diskoConfigurations.${hostname} ];
      facter.reportPath = facterReportPath;
    };
    users.${config.flake.meta.username}.modules = config.unify.hosts.nixos.${hostname}.modules;
  };

  flake.diskoConfigurations.${hostname} = diskoConfig { inherit (config.flake.meta) persistDir; };
}
