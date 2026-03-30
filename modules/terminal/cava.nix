{
  unify.modules.general.home =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      catppuccin.cava.transparent = true;

      programs.cava = {
        enable = true;
        settings = {
          general.sleep_timer = 1;
          output = {
            show_idle_bar_heads = false;
            orientation = "horizontal";
            # Rather than showing both channels, such that they mirror each other,
            # show the average of them as a single channel
            channels = "mono";
          };
          smoothing.monstercat = 1;
        };
      };

      xdg.autostart.entries = lib.singleton (
        pkgs.makeDesktopItem {
          name = "cava-wallpaper";
          desktopName = "Cava Wallpaper";
          exec = "${lib.getExe pkgs.windowtolayer} handlr launch x-scheme-handler/terminal -- --background-opacity=0 --font-size=3 -e ${lib.getExe config.programs.cava.package}";
          # Make it more concise to get path to desktop file
          destination = "/";
        }
        + "/cava-wallpaper.desktop"
      );
    };
}
