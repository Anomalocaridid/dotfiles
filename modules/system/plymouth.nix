{
  unify.modules.general.nixos.boot = {
    plymouth.enable = true;
    # Below settings are necessar to show graphical LUKS password prompt
    # Silence first boot output
    consoleLogLevel = 3;
    initrd = {
      verbose = false;
      systemd.enable = true;
    };
    kernelParams = [
      "quiet"
      "splash"
      "intremap=on"
      "boot.shell_on_fail"
      "udev.log_priority=3"
      "rd.systemd.show_status=auto"
    ];
  };
}
