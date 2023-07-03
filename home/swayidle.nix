{ pkgs, ... }: {
  services.swayidle =
    let
      lockCommand = "${pkgs.custom.lockman}/bin/lockman.sh &";
    in
    {
      enable = true;
      systemdTarget = "hyprland-session.target";
      timeouts =
        let
          dpmsCommand = "${pkgs.hyprland}/bin/hyprctl dispatch dpms";
        in
        [
          {
            timeout = 300;
            command = lockCommand;
          }
          {
            timeout = 600;
            command = "${dpmsCommand} off";
            resumeCommand = "${dpmsCommand} on";
          }
        ];
      events = [
        {
          event = "before-sleep";
          command = lockCommand;
        }
      ];
    };
}
