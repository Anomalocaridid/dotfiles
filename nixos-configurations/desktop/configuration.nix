{ diskoConfigurations, ... }:
{
  networking.hostName = "desktop";
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    diskoConfigurations.desktop
  ];
}
