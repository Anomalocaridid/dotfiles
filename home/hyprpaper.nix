{ config, lib, ... }:
{
  services.hyprpaper = {
    enable = true;
    settings =
      let
        palette =
          (lib.importJSON "${config.catppuccin.sources.palette}/palette.json")
          .${config.catppuccin.flavor}.colors;
      in
      {
        splash = true;
        splash_offset = 6.5e-2;
        splash_color = palette.text.hex;

        "$wallpaper" = config.stylix.image;
        preload = [ "$wallpaper" ];
        wallpaper = [ ",$wallpaper" ];
      };
  };
}
