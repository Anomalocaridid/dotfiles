{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  palette =
    (lib.importJSON "${config.catppuccin.sources.palette}/palette.json")
    .${config.catppuccin.flavor}.colors;
in
{
  # Needed for weird technical reasons because `home.stateVersion` < 23.05
  catppuccin.swaylock.enable = true;
  programs.swaylock = {
    enable = true;
    package = pkgs.swaylock-plugin;
    settings = {
      indicator-caps-lock = true;
      font-size = 20;
      ignore-empty-password = true;
      show-failed-attempts = true;
      indicator-radius = 115;

      # Make background color in ring opaque
      inside-color = lib.mkForce "${palette.base.hex}";
      inside-ver-color = lib.mkForce "${palette.base.hex}";
      inside-caps-lock-color = lib.mkForce "${palette.base.hex}";
      inside-wrong-color = lib.mkForce "${palette.base.hex}";
      inside-clear-color = lib.mkForce "${palette.base.hex}";

      # swaylock-plugin-specific settings
      grace = 2;
      command-each = lib.getExe (
        pkgs.writeShellApplication {
          name = "lockman.sh";
          runtimeInputs = with pkgs; [
            handlr-regex
            windowtolayer
            asciiquarium-transparent
            cbonsai
            fastfetch
            lavat
            pipes-rs
            pv
            sssnake
            ternimal
            unimatrix
            util-linux # provides script
          ];
          text =
            # bash
            ''
              readonly SCREENSAVERS=(
                "asciiquarium --transparent"
                "cbonsai --live --infinite"
                "lavat -s 10 -c red -k magenta"
                "pipes-rs"
                "sssnake --mode=screensaver --speed=20 --try-hard=1"
                # ternimal needs the terminal dimensions to be explicitly passed
                # tput calls should only be evaluated by terminal
                "ternimal width=\$(tput cols) height=\$((\$(tput lines) * 2))"
                "unimatrix --asynchronous --flashers"
                # script makes fastfetch think it is outputting to a terminal, which is necessary to preserve colors
                # pv throttles the data so it looks like it is being typed out
                "while true; do script --quiet --log-out /dev/null --command fastfetch | pv -qL 200; done"
              )

              # Each shader matches up with a wallpaper
              readonly SHADERS=(
                "water"
                "bloom"
                "cineShader-Lava"
                "gears-and-belts"
                "cubes"
                "sin-interference"
                "matrix-hallway"
                "in-game-crt"
              )

              index=$((RANDOM % ''${#SCREENSAVERS[@]}))

              # sleep for a bit so the terminal has time to set its dimensions
              timeout 60 windowtolayer handlr launch x-scheme-handler/terminal -- --custom-shader="${inputs.ghostty-shaders}/''${SHADERS[$index]}.glsl" -e "sleep 0.2 && ''${SCREENSAVERS[$index]}"
            '';
        }
      );
    };
  };

  # Screensaver config
  xdg.configFile."pipes-rs/config.toml".source =
    (inputs.nixago.lib.${pkgs.system}.make {
      data = {
        color_mode = "rgb";
        rainbow = 1;
        kinds = [
          "heavy"
          "light"
          "curved"
          "outline"
        ];
        num_pipes = 2;
      };
      output = "config.toml";
    }).configFile;
}
