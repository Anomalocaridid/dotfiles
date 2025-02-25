{ ... }:
{
  # Use stylix to automatically set wallpaper
  # Stylix.autoenable is set to false
  stylix.targets.wpaperd.enable = true;

  services.wpaperd.enable = true;
}
