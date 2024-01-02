{ ... }: {
  programs.hyprland.enable = true;
  # Tell electron apps to use Wayland
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  # Hyprland binary cache
  nix.settings = {
    substituters = [ "https://hyprland.cachix.org" ];
    trusted-public-keys = [ "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc=" ];
  };
}
