{ config, inputs, ... }:
{
  flake.meta.username = "anomalocaris";

  flake-file.inputs.hpf-passwd = {
    url = "github:Anomalocaridid/hpf-passwd";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  unify.modules.general =
    let
      inherit (config.flake.meta) username persistDir passwordDir;
    in
    {
      nixos =
        { pkgs, hostConfig, ... }:
        {
          users = {
            # Prevent changing users and groups outside of this config
            mutableUsers = false;

            users.${username} = {
              isNormalUser = true;
              # Enable ‘sudo’ for the user
              extraGroups = [ "wheel" ];
              hashedPasswordFile = "${passwordDir}/${username}";
            };
          };

          # persistDir is needed for boot because it contains password hashes
          fileSystems.${persistDir}.neededForBoot = true;
        };

      home =
        { pkgs, ... }:
        {
          home = {
            username = username;
            homeDirectory = "/home/${username}";

            packages = [ inputs.hpf-passwd.packages.${pkgs.stdenv.hostPlatform.system}.hpf-passwd ];
          };
        };
    };
}
