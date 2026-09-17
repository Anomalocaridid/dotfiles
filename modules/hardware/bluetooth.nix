{
  flake.modules.nixos.general = {
    # Enable bluetooth
    hardware.bluetooth.enable = true;
    # Bluetooth manager
    services.blueman.enable = true;
  };
}
