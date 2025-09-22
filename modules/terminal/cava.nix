{
  unify.modules.general.home =
    { lib, pkgs, ... }:
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
    };
}
