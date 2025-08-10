{ inputs, ... }:
{
  flake.modules.homeManager.wpaperd.services.wpaperd = {
    enable = true;
    settings.any.path = inputs.catppuccin-fractal-wallpapers + "/05.png";
  };
}
