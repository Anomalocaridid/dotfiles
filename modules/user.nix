{ config, ... }:
{
  flake.meta.username = "anomalocaris";

  unify.modules.user =
    let
      inherit (config.flake.meta) username persistDir passwordDir;
    in
    {
      nixos =
        { pkgs, hostConfig, ... }:
        {
          users.users = {
            ${username} = {
              shell = pkgs.fish;
              isNormalUser = true;
              extraGroups = [
                "wheel" # Enable ‘sudo’ for the user
                "networkmanager" # Change network settings
              ];
              hashedPasswordFile = "${passwordDir}/${username}";
            };
            root.hashedPasswordFile = "${passwordDir}/root";
          };

          # persistDir is needed for boot because it contains password hashes
          fileSystems.${persistDir}.neededForBoot = true;
        };

      home.home.username = username;
    };
}
