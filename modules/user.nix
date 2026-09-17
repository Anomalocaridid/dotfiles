{ config, inputs, ... }:
let
  inherit (config.flake.meta) username persistDir passwordDir;
in
{
  flake-file.inputs.hpf-passwd = {
    url = "github:Anomalocaridid/hpf-passwd";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  flake = {
    meta.username = "anomalocaris";

    modules = {
      nixos.default = {
        users = {
          # Prevent changing users and groups outside of this config
          mutableUsers = false;

          users.${username} = {
            isNormalUser = true;
            home = "/home/${username}";
            # Enable ‘sudo’ for the user
            extraGroups = [ "wheel" ];
            hashedPasswordFile = "${passwordDir}/${username}";
          };
        };

        # persistDir is needed for boot because it contains password hashes
        fileSystems.${persistDir}.neededForBoot = true;
      };

      homeManager.primaryUser = { pkgs, ... }: {
        home.packages = [ inputs.hpf-passwd.packages.${pkgs.stdenv.hostPlatform.system}.hpf-passwd ];
      };
    };
  };
}
