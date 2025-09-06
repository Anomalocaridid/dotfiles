{ config, ... }:
let
  inherit (config.flake.meta) username persistDir;
in
{
  unify.modules.general = {
    nixos = {
      # Enable KVM nested virtualization
      boot.extraModprobeConfig = "options kvm_amd nested=1";

      # persist quickemu VMs
      environment.persistence.${persistDir}.users.${username}.directories = [ "quickemu" ];
    };

    home =
      { pkgs, ... }:
      {
        # Utilities to easily make and run vms
        home.packages = with pkgs; [ quickemu ];
      };
  };
}
