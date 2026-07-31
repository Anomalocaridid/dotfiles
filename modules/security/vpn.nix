{ config, ... }:
let
  inherit (config.flake.meta) username persistDir;
in
{
  unify = {
    # Allow wireguard through firewall
    nixos.networking.firewall = {
      # if packets are still dropped, they will show up in dmesg
      logReversePathDrops = true;
      # wireguard trips rpfilter up
      extraCommands = ''
        ip46tables -t mangle -I nixos-fw-rpfilter -p udp -m udp --sport 51820 -j RETURN
        ip46tables -t mangle -I nixos-fw-rpfilter -p udp -m udp --dport 51820 -j RETURN
      '';
      extraStopCommands = ''
        ip46tables -t mangle -D nixos-fw-rpfilter -p udp -m udp --sport 51820 -j RETURN || true
        ip46tables -t mangle -D nixos-fw-rpfilter -p udp -m udp --dport 51820 -j RETURN || true
      '';
    };

    modules.general = {
      # Persist Proton VPN settings
      nixos.environment.persistence.${persistDir}.users.${username}.directories = [
        # Contains cached server list, which can cause issues if not present and advanced kill switch is on
        ".cache/Proton/VPN"
        # Proton VPN settings
        ".config/Proton/VPN"
        # Proton VPN relies on keyring, so this needs to be persisted to stay logged in across sessions
        {
          directory = ".local/share/keyrings";
          mode = "u=rwx,g=,o=";
        }
      ];

      home =
        {
          lib,
          pkgs,
          osConfig,
          ...
        }:
        {
          home.packages = with pkgs; [ proton-vpn ];

          xdg.autostart.entries = lib.singleton (
            pkgs.makeDesktopItem {
              name = "proton-vpn-autostart";
              desktopName = "Proton VPN (Autostart)";
              exec = "${lib.getExe pkgs.proton-vpn} --start-minimized";
              # Make it more concise to get path to desktop file
              destination = "/";
            }
            + "/proton-vpn-autostart.desktop"
          );

        };
    };
  };
}
