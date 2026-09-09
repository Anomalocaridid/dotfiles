{
  unify.modules.appliance.nixos.boot = {
    plymouth.enable = true;

    # Enable "silent boot"
    consoleLogLevel = 3;
    initrd.verbose = false;
    kernelParams = [
      "quiet"
      "rd.udev.log_level=3"
      "rd.systemd.show_status=auto"
      # Do not show serial console output
      # Needed on raspberry pi
      "plymouth.ignore-serial-consoles"
    ];
  };
}
