{
  hostname,
  diskoConfig,
  facterReportPath,
}:
{ config, lib, ... }:
rec {
  unify.hosts.nixos.${hostname} = {
    modules = lib.attrsets.attrValues config.unify.modules;
    nixos = {
      networking.hostName = hostname;
      imports = [ flake.diskoConfigurations.${hostname} ];
      facter.reportPath = facterReportPath;
    };
    users.anomalocaris.modules = config.unify.hosts.nixos.${hostname}.modules;
  };

  flake.diskoConfigurations.${hostname} = diskoConfig;
}
