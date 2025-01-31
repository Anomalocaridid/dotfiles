{
  lib,
  pkgs,
  config,
  ...
}:
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
          # NOTE: Run ignis commands outside of swaylock-plugin because nested compositor causes issues
          # and run window manager commands outside of swaylock-plugin so the first one is only run once even if screensaver changes
          # TODO: generalize ignis commands to multiple monitors
          command = lib.getExe (
            pkgs.writeShellApplication {
              name = "swaylock-wrapper.sh";
              runtimeInputs = with pkgs; [
                niri
                ignis
              ];
              text = ''
                # Go to empty workspace (last one is always empty)
                niri msg action focus-workspace 255
                ignis close ignis_bar_0
                ${lib.getExe config.programs.swaylock.package}
                ignis open ignis_bar_0
                niri msg action focus-workspace-previous
              '';
            }
          );
        }
        {
          event = "before-sleep";
          command = lock;
        }
      ];
    };
}
