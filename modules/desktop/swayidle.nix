{
  unify.modules.general.home =
    { config, lib, ... }:
    {
      # Hack to ensure screen locker works properly
      # Otherwise, nothing would be accessible from swayidle's environment
      systemd.user.services.swayidle.Service.Environment = lib.mkForce [ ];
      services.swayidle =
        let
          lock = "loginctl lock-session";
          dpms = status: "niri msg action power-${status}-monitors";
        in
        {
          enable = true;
          timeouts = [
            # Lock after 5 minutes
            {
              timeout = 300;
              command = lock;
            }
            # Turn screen off after 5 minutes, 30 seconds
            {
              timeout = 330;
              command = dpms "off";
              resumeCommand = dpms "on";
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
              # NOTE: niri does not support transparency for lock screens
              command = lib.getExe config.programs.swaylock.package;
            }
            {
              event = "before-sleep";
              command = lock;
            }
          ];
        };
    };
}
