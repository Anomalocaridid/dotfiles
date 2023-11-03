{ config, lib, pkgs, inputs, ... }: {
  programs.swaylock = {
    enable = true;
    package = pkgs.swaylock-effects;
    settings =
      let
        fonts = config.stylix.fonts;
        palette = pkgs.custom.catppuccin-palette.${config.catppuccin.flavour};
        themeFile = inputs.catppuccin-swaylock + /themes/${config.catppuccin.flavour}.conf;
        theme = lib.trivial.pipe
          (pkgs.runCommand "catppuccin-swaylock-theme" { } ''
            ${pkgs.jc}/bin/jc --ini < ${themeFile} > $out
          '')
          [
            builtins.readFile
            builtins.fromJSON
          ];
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
