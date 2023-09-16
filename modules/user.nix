{ pkgs, ... }:
let
  user = "anomalocaris";
  persistDir = "/persist";
  passwordDir = "/${persistDir}/passwords";
in
{
  users.users = {
    ${user} = {
      shell = pkgs.zsh;
      isNormalUser = true;
      extraGroups = [
        "wheel" # Enable ‘sudo’ for the user
        "libvirtd" # Allow access to virt-manager
        "networkmanager" # Change network settings
      ];
      hashedPasswordFile = "${passwordDir}/${user}";
    };
    root.hashedPasswordFile = "${passwordDir}/root";
  };

  # Ensure certain directories have necessary permissions
  systemd.tmpfiles.rules = [
    "Z ${persistDir}/etc/nixos    -    ${user} users"
    "d ${persistDir}/home/${user} 0755 ${user} users"
  ];


  # /persist is needed for boot because it contains password hashes
  # TODO: See if this line can be moved to disko config
  fileSystems.${persistDir}.neededForBoot = true;

  # Need to enable zsh at system level to use as shell
  programs.zsh.enable = true;
}
