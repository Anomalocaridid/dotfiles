{ config, ... }:
{
  flake.meta.username = "anomalocaris";

  unify.modules.general =
    let
      inherit (config.flake.meta) username persistDir passwordDir;
    in
    {
      nixos =
        { pkgs, hostConfig, ... }:
        {
          users.users.${username} = {
            isNormalUser = true;
            # Enable ‘sudo’ for the user
            extraGroups = [ "wheel" ];
            hashedPasswordFile = "${passwordDir}/${username}";
          };

          # persistDir is needed for boot because it contains password hashes
          fileSystems.${persistDir}.neededForBoot = true;
        };

      home.home = {
        username = username;
        homeDirectory = "/home/${username}";
      };
    };
}
