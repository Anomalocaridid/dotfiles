{
  config,
  lib,
  osConfig,
  ...
}:
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
        splash_offset = 0.25;
        splash_color = palette.text.hex;

        "$wallpaper" = osConfig.stylix.image;
        preload = [ "$wallpaper" ];
        wallpaper = [ ",$wallpaper" ];
      };
  };
}
