{ diskoConfigurations, ... }:
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    diskoConfigurations.home-pc
  ];
}
