{
  unify.modules.udiskie = {
    # Required for udiskie
    nixos.services.udisks2.enable = true;
    home =
      { lib, pkgs, ... }:
      {
        services.udiskie = {
          enable = true;
          settings.program_options = {
            terminal = "xterm";
            event_hook = "${lib.getExe' pkgs.libnotify "notify-send"} --icon=drive-removable-media {event} {device_presentation}";
          };
        };
      };
  };
}
