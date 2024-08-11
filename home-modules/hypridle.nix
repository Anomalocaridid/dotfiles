{ lib, pkgs, ... }:
{
  services.hypridle =
    let
      lock = "loginctl lock-session";
      dpms = "${lib.getExe' pkgs.hyprland "hyprctl"} dispatch dpms";
    in
    {
      enable = true;

      settings = {
        general = {
          # Only start one instance of locking script
          lock_cmd = "pidof -x lockman.sh || ${lib.getExe pkgs.custom.lockman}";
          # Lock before suspend
          before_sleep_md = lock;
          # Avoid having to press multiple keys to turn on screen
          after_sleep_cmd = "${dpms} on";
        };

        listener = [
          # Lock after 5 minutes
          {
            timeout = 300;
            on-timeout = lock;
          }
          # Turn screen off after 5 minutes, 30 seconds
          {
            timeout = 330;
            on-timeout = "${dpms} off";
            on-resume = "${dpms} on";
          }
          # Hibernate system after 30 minutes
          {
            timeout = 1800;
            on-timeout = "systemctl hibernate";
          }
        ];
      };
    };
}
