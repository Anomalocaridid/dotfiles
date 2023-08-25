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
}
