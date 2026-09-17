{ config, ... }:
let
  inherit (config.flake.meta) username persistDir;
in
{
  flake.modules = {
    nixos.general = {
      # Enable KVM nested virtualization
      boot.extraModprobeConfig = "options kvm_amd nested=1";

      # persist quickemu VMs
      environment.persistence.${persistDir}.users.${username}.directories = [ "quickemu" ];
    };

    homeManager.general = { pkgs, ... }: {
      # Utilities to easily make and run vms
      home.packages = with pkgs; [ quickemu ];
    };
  };
}
