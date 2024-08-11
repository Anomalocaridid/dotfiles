{ pkgs, ... }:
{
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
    sudo = {
      package = pkgs.sudo.override { withInsults = true; };
      extraConfig = # sudo
        ''
          # Prevents sudo lecture from appearing after reboot without persisting
          Defaults lecture = never
          # Sudo insults after failed attempts because why not
          Defaults insults
        '';
    };
    # Needed for hyprlock to unlock
    pam.services.hyprlock = { };
  };
}
