{ ... }:
{
  programs.hyprland.enable = true;
  # Tell electron apps to use Wayland
  environment.sessionVariables.NIXOS_OZONE_WL = "1";
}
