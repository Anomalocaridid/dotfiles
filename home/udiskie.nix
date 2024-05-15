{ lib, pkgs, ... }:
{
  services.udiskie = {
    enable = true;
    settings.program_options = {
      terminal = "xterm";
      notify_command = "${lib.getExe' pkgs.libnotify "notify-send"} --icon=drive-removable-media {{event}} {{device_presentation}}";
    };
  };
}
