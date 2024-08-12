{
  lib,
  pkgs,
  config,
  ...
}:
{
  # Use stylix to automatically set wallpaper
  stylix.targets.hyprpaper.enable = true;

  services.hyprpaper = {
    enable = true;
    settings =
      let
        # TODO: remove and replace with source if hyprpaper adds ability to source hyprland
        palette = builtins.fromJSON (
          builtins.readFile (
            pkgs.runCommand "converted.json" { } ''
              ${lib.getExe pkgs.jc} --ini < ${
                config.catppuccin.sources.hyprland + /themes/${config.catppuccin.flavor}.conf
              } > $out
            ''
          )
        );
      in
      {
        splash = true;
        splash_offset = 6.5e-2;
        splash_color = palette."$text";

        # Disable ipc because I do not need it and it constantly ticks
        ipc = "off";
      };
  };
}
