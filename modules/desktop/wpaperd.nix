{ config, inputs, ... }:
{
  flake.meta.wallpaper = inputs.catppuccin-fractal-wallpapers + "/05.png";

  unify.modules.general.home.services.wpaperd = {
    enable = true;
    settings.any.path = config.flake.meta.wallpaper;
  };
}
