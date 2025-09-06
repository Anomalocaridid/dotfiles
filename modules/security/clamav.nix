{ config, ... }:
{
  unify.modules.general.nixos = {
    services.clamav = {
      daemon.enable = true;
      updater.enable = true;
      # Unofficial updater for extra malware signatures
      fangfrisch.enable = true;
    };

    # ClamAV signature database
    environment.persistence.${config.flake.meta.persistDir}.directories = [ "/var/lib/clamav" ];
  };
}
