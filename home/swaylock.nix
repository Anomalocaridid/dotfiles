{ config, pkgs, ... }: {
  programs.swaylock = {
    enable = true;
    package = pkgs.swaylock-effects;
    settings =
      let
        fonts = config.stylix.fonts;
        palette = pkgs.custom.catppuccin-palette.${config.catppuccin.flavour};
        theme = pkgs.custom.parseTheme {
          name = "swaylock";
          themeFile = pkgs.fetchFromGitHub
            {
              owner = "remiposo";
              repo = "swaylock";
              rev = "9b88d9e866c044d47c98046ee6c8d6de2546cf82";
              hash = "sha256-v2op7V52VYqzY9govnfkgmF7ybRRlPkohgnrUWDjItI=";
            } + "/themes/${config.catppuccin.flavour}.conf";
          keyField = 1;
          valueField = 2;
          fs = "=";
        };
      in
      theme // {
        indicator-caps-lock = true;
        font = "${fonts.sansSerif.name}";
        font-size = 20;
        ignore-empty-password = true;
        show-failed-attempts = true;
        color = "#00000000";

        # Ring
        indicator-radius = 115;

        # Inside background color
        inside-color = "#${palette.base.hex}";
        inside-ver-color = "#${palette.base.hex}";
        inside-caps-lock-color = "#${palette.base.hex}";
        inside-wrong-color = "#${palette.base.hex}";
        inside-clear-color = "#${palette.base.hex}";

        # Swaylock-effects specific settings
        clock = true;
        timestr = "%r";
        grace = 2;
      };
  };

  # Screensaver config
  xdg.configFile."pipes-rs/config.toml".source =
    let
      tomlFormat = pkgs.formats.toml { };
    in
    tomlFormat.generate "pipes-rs-config" {
      # bold = true;
      color_mode = "rgb"; # ansi, rgb or none
      # palette = "default"; # default, darker, pastel or matrix
      rainbow = 1; # 0-255
      # delay_ms = 20;
      # inherit_style = false;
      kinds = [
        "heavy"
        "light"
        "curved"
        "outline"
      ]; # heavy, light, curved, knobby, emoji, outline, dots, blocks, sus
      num_pipes = 2;
      # reset_threshold = 0.5; # 0.0–1.0
      # turn_chance = 0.15; # 0.0–1.0
    };
}
