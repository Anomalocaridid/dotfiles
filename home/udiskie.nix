{ pkgs, ... }: {
  services.udiskie = {
    enable = true;
    settings.program_options = {
      terminal = "xterm";
      notify_command = "${pkgs.libnotify}/bin/notify-send --icon=drive-removable-media {{event}} {{device_presentation}}";
    };
  };
}
