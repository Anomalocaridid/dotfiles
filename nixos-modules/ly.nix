{ ... }:
{
  services.displayManager.ly = {
    enable = true;
    settings = {
      animation = "doom";
      blank_password = true;
      hide_borders = true;
    };
  };

  # Ensure services start properly
  systemd.services.display-manager.environment.XDG_CURRENT_DESKTOP = "X-NIXOS-SYSTEMD-AWARE";
}
