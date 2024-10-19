{ pkgs, ... }:
{
  services.displayManager.ly = {
    enable = true;
    settings = {
      animation = "doom";
      blank_password = true;
      hide_borders = true;
    };
  };

  # Set default session
  environment.etc."ly/save.ini".source =
    let
      iniFormat = pkgs.formats.iniWithGlobalSection { };
    in
    iniFormat.generate "save.ini" {
      globalSection = {
        user = "anomalocaris";
        session_index = 2;
      };
    };

  # Ensure services start properly
  systemd.services.display-manager.environment.XDG_CURRENT_DESKTOP = "X-NIXOS-SYSTEMD-AWARE";
}
