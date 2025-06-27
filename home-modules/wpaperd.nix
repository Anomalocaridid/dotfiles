{ inputs, ... }:
{
  services.wpaperd = {
    enable = true;
    settings.any = {
      path = inputs.catppuccin-fractal-wallpapers + "/05.png";
    };
  };
}
