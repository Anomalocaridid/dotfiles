{
  unify.modules.general.nixos = {
    # Enable bluetooth
    hardware.bluetooth.enable = true;
    # Bluetooth manager
    services.blueman.enable = true;
  };
}
