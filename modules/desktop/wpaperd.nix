{ config, inputs, ... }:
{
  flake.meta.wallpaper = inputs.catppuccin-fractal-wallpapers + "/05.png";

  # Catppuccin wallpapers
  flake-file.inputs.catppuccin-fractal-wallpapers = {
    url = "github:psylopneunonym/Catppuccin-Fractal-Wallpapers";
    flake = false;
  };

  unify.modules.general.home.services.wpaperd = {
    enable = true;
    settings.any.path = config.flake.meta.wallpaper;
  };
}
