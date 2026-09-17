{
  flake.modules = {
    # Required for udiskie
    nixos.general.services.udisks2.enable = true;

    homeManager.general = { lib, pkgs, ... }: {
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
