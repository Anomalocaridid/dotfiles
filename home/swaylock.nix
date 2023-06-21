{ pkgs, ... }: {
  programs.swaylock = {
    enable = true;
    package = pkgs.swaylock-effects;
    settings = {
      indicator-caps-lock = true;
      font = "Fira Code Nerd Font";
      font-size = 20;
      ignore-empty-password = true;
      show-failed-attempts = true;
      color = "#00000000";

      # Ring
      indicator-radius = 115;

      # Typing 
      line-color = "#0abdc6";
      text-color = "#0abdc6";
      inside-color = "#000b1e";
      ring-color = "#000b1e";
      separator-color = "#ea00d9";
      key-hl-color = "#ea00d9";
      bs-hl-color = "#ff0000";

      # Verifying
      line-ver-color = "#0abdc6";
      text-ver-color = "#0abdc6";
      inside-ver-color = "#091833";
      ring-ver-color = "#133e7c";

      # Caps Lock
      line-caps-lock-color = "#f57800";
      text-caps-lock-color = "#f57800";
      inside-caps-lock-color = "#f5780050";
      ring-caps-lock-color = "#f57800";
      caps-lock-key-hl-color = "#ffff00";
      caps-lock-bs-hl-color = "#ff0000";

      # Wrong
      line-wrong-color = "#ff0000";
      text-wrong-color = "#ff0000";
      inside-wrong-color = "#ff000040";
      ring-wrong-color = "#ff0000";

      # Clear
      line-clear-color = "#00ff00";
      text-clear-color = "#00ff00";
      inside-clear-color = "#00ff0040";
      ring-clear-color = "#00ff00";

      # Swaylock-effects specific settings
      clock = true;
      timestr = "%r";
      grace = 2;
    };
  };
  home.packages = with pkgs;[
    (writeShellApplication {
      name = "lockman.sh";
      runtimeInputs = [ pipes-rs ];
      text = ''
        # Move to empty workspace and run screensaver
        hyprctl dispatch workspace empty
        handlr launch x-scheme-handler/terminal -- pipes-rs
        # Fullscreen screensaver
        sleep 0.1 # slight delay needed for fullscreen to work
        hyprctl dispatch fullscreen 0
        # Lock screen
        swaylock
        hyprctl --batch "dispatch killactive ; dispatch workspace previous"
      '';
    })
  ];
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
