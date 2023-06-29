{ pkgs, ... }: {
  services = {
    # Enable ClamAV
    clamav = {
      daemon.enable = true;
      updater.enable = true;
    };
    # Enable Yubikey support
    pcscd.enable = true;
  };

  security = {
    sudo.extraConfig = #sudo
      ''
        # Prevents sudo lecture from appearing after reboot without persisting
        Defaults lecture = never
        # Sudo insults after failed attempts because why not
        Defaults insults
      '';
    # Needed for swaylock to unlock
    pam.services.swaylock = { };
  };
  # Create ClamAV signature database if it does not exist.
  systemd.services.freshclam-init =
    let
      clamavServices = [ "clamav-daemon.service" ];
    in
    {
      description = "Create ClamAV signature database";
      before = clamavServices;
      script = "${pkgs.clamav}/bin/freshclam";
      unitConfig.ConditionPathExists = "!/var/lib/clamav";
      serviceConfig.Type = "oneshot";
      requiredBy = clamavServices;
    };
}
