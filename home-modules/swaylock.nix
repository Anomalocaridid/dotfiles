{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  fonts = config.stylix.fonts;
  palette =
    (lib.importJSON "${config.catppuccin.sources.palette}/palette.json")
    .${config.catppuccin.flavor}.colors;
  # Randomly picks a screensaver
  # Has to be its own script so it can sleep for a bit and give the terminal time to set dimensions
  pick-screensaver = pkgs.writeShellApplication {
    name = "pick-screensaver.sh";
    runtimeInputs = with pkgs; [
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
    text = ''
      # Give terminal a bit of time to properly set dimensions
      # Needs to be before the declaration of $SCREENSAVERS because some screensaver commands rely on it
      sleep 0.2

      readonly SCREENSAVERS=(
        "asciiquarium --transparent"
        "cbonsai --live --infinite"
        "lavat -s 10 -c red -k magenta"
        "pipes-rs"
        "sssnake --mode=screensaver --speed=20 --try-hard=1"
        "ternimal width=$(tput cols) height=$(($(tput lines) * 2))"
        "unimatrix --asynchronous --flashers"
        # script makes fastfetch think it is outputting to a terminal, which is necessary to preserve colors
        "while true; do script --quiet --log-out /dev/null  --command fastfetch | pv -qL 200; done"
      )

      # eval is needed to use shell features in screensaver commands
      eval "''${SCREENSAVERS[(($RANDOM % ''${#SCREENSAVERS[@]}))]}"
    '';
  };
in
{
  # Needed for weird technical reasons because `home.stateVersion` < 23.05
  catppuccin.swaylock.enable = true;
  programs.swaylock = {
    enable = true;
    package = pkgs.custom.swaylock-plugin;
    settings = {
      indicator-caps-lock = true;
      font = fonts.sansSerif.name;
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
            custom.windowtolayer
            pick-screensaver
          ];
          text =
            # bash
            ''
              timeout 60 windowtolayer handlr launch x-scheme-handler/terminal -- -e pick-screensaver.sh
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
