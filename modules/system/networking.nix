{ config, ... }:
let
  inherit (config.flake.meta) username persistDir;
in
{
  flake.modules.nixos.default = { lib, ... }: {
    networking.networkmanager.enable = true;

    # Let user change network settings with networkmanager
    users.users.${username}.extraGroups = [ "networkmanager" ];

    # Prevent nixos-rebuild from freezing until this times out
    systemd.network.enable = lib.mkForce false;

    # Persist network connections
    environment.persistence.${persistDir}.directories = [ "/etc/NetworkManager/system-connections" ];
  };
}
