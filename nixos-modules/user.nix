{ pkgs, ... }:
let
  user = "anomalocaris";
  persistDir = "/persist";
  passwordDir = "/${persistDir}/passwords";
in
{
  users.users = {
    ${user} = {
      shell = pkgs.fish;
      isNormalUser = true;
      extraGroups = [
        "wheel" # Enable ‘sudo’ for the user
        "networkmanager" # Change network settings
      ];
      hashedPasswordFile = "${passwordDir}/${user}";
    };
    root.hashedPasswordFile = "${passwordDir}/root";
  };

  # Ensure certain directories exist and have necessary permissions
  systemd.tmpfiles.rules = [
    "Z ${persistDir}/etc/nixos                -    ${user} users"
    "d ${persistDir}/home/${user}             0755 ${user} users"
    "Z ${persistDir}/home/${user}             -    ${user} users"
  ];

  # /persist is needed for boot because it contains password hashes
  # TODO: See if this line can be moved to disko config
  fileSystems.${persistDir}.neededForBoot = true;

  # Need to enable fish at system level to use as shell
  programs.fish = {
    enable = true;
    useBabelfish = true;
  };
}
