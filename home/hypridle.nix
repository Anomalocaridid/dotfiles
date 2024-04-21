{ pkgs, ... }:
{
  services.hypridle =
    let
      lock = "loginctl lock-session";
      dpms = "${pkgs.hyprland}/bin/hyprctl dispatch dpms";
    in
    {
      enable = true;

      # Only start one instance of locking script
      lockCmd = "pidof -x lockman.sh || ${pkgs.custom.lockman}/bin/lockman.sh";
      # Lock before suspend
      beforeSleepCmd = lock;
      # Avoid having to press multiple keys to turn on screen
      afterSleepCmd = "${dpms} on";

      listeners = [
        # Lock after 5 minutes
        {
          timeout = 300;
          onTimeout = lock;
        }
        # Turn screen off after 5 minutes, 30 seconds
        {
          timeout = 330;
          onTimeout = "${dpms} off";
          onResume = "${dpms} on";
        }
        # Hibernate system after 30 minutes
        {
          timeout = 1800;
          onTimeout = "systemctl hibernate";
        }
      ];
    };
}
