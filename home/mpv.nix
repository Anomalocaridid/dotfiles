{ pkgs, ... }: {
  home.packages = with pkgs; [ mpv ];

  xdg.configFile =
    let
      kvFormat = pkgs.formats.keyValue { };
      font = "FiraCode Nerd Font";
    in
    {
      "mpv/mpv.conf".source = ../dotfiles/.config/mpv/mpv.conf;
      "mpv/script-opts/console.conf".source = kvFormat.generate "mpv-script-opts-console" {
        font = font;
      };
      "mpv/script-opts/osc.conf".source = kvFormat.generate "mpv-script-opts-osc" {
        seekbarstyle = "diamond";
      };
      "mpv/script-opts/stats.conf".source = kvFormat.generate "mpv-script-opts-stats" {
        font = font;
        font_mono = font;
        #BBGGRR
        font_color = "C6BD0A";
        border_color = "1E0B00";
        plot_bg_border_color = "D900EA";
        plot_bg_color = "331809";
        plot_color = "D900EA";
      };
    };
}
