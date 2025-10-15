{ inputs, ... }:
{
  # Shaders for ghostty terminal
  flake-file.inputs.ghostty-shaders = {
    url = "github:hackr-sh/ghostty-shaders";
    flake = false;
  };

  unify.modules.general = {
    # Needed for swaylock-plugin to unlock
    nixos.security.pam.services.swaylock-plugin = { };
    home =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      let
        palette = config.catppuccin.sources.parsedPalette;
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
                name = "screensaver.sh";
                runtimeInputs = with pkgs; [
                  ghostty # Heavily relies on ghostty-specific features
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
                  util-linux # Provides `script`
                ];
                text =
                  # bash
                  ''
                    # TODO: figure out how to get rid of sleep
                    readonly SCREENSAVERS=(
                      "asciiquarium --transparent"
                      "cbonsai --live --infinite"
                      # sleep required or lavat will be very slow
                      "sleep 0.02 && lavat -s 10 -c red -k magenta"
                      "pipes-rs"
                      # sleep required or sssnake will instantly terminate
                      "sleep 0.02 && sssnake --mode=screensaver --speed=20 --try-hard=1"
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

                    # TODO: figure out why using `timeout` to periodically change screensavers tends to fail
                    windowtolayer ghostty --custom-shader="${inputs.ghostty-shaders}/''${SHADERS[$index]}.glsl" -e sh -c "''${SCREENSAVERS[$index]}"
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
      };
  };
}
