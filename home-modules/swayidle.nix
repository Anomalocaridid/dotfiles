{ lib, pkgs, ... }:
{
  services.swayidle =
    let
      lock = "loginctl lock-session";
      dpms = "${lib.getExe' pkgs.hyprland "hyprctl"} dispatch dpms";
    in
    {
      enable = true;
      systemdTarget = "hyprland-session.target";
      timeouts = [
        # Lock after 5 minutes
        {
          timeout = 300;
          command = lock;
        }
        # Turn screen off after 5 minutes, 30 seconds
        {
          timeout = 330;
          command = "${dpms} off";
          resumeCommand = "${dpms} on";
        }
        # Hibernate system after 30 minutes
        {
          timeout = 1800;
          command = "systemctl hibernate";
        }
      ];
      events = [
        {
          event = "lock";
          # Only start one instance of locking script
          command = "pidof -x lockman.sh || ${lib.getExe pkgs.custom.lockman}";
        }
        {
          event = "before-sleep";
          command = lock;
        }
      ];
    };
}
