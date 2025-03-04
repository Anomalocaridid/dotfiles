{ diskoConfigurations, ... }:
{
  networking.hostName = "laptop";
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    diskoConfigurations.laptop
  ];
}
