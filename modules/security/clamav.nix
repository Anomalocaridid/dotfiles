{
  flake.modules.nixos.clamav.services.clamav = {
    daemon.enable = true;
    updater.enable = true;
    # Unofficial updater for extra malware signatures
    fangfrisch.enable = true;
  };
}
