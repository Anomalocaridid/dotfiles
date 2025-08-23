{ inputs, ... }:
{
  unify.modules.niri = {
    nixos = {
      imports = [ inputs.niri.nixosModules.niri ];
      programs.niri.enable = true;
    };
    home =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        programs.niri.settings =
          let
            launch = mime: [
              "handlr"
              "launch"
              mime
              "--"
            ];
            terminal = launch "x-scheme-handler/terminal";
            palette = config.catppuccin.sources.parsedPalette;
            accent = palette.${config.catppuccin.accent}.hex;
            niriSettings = config.programs.niri.settings;
            workspaces = 10;
          in
          {
            # Prefer no client-side decorations to make borders and rounded corners a bit more consistent
            prefer-no-csd = true;

            # Needed for xwayland-satellite
            environment.DISPLAY = ":0";

            hotkey-overlay.skip-at-startup = true;

            layout = {
              always-center-single-column = true;

              # Show backdrop as wallpaper
              background-color = "transparent";

              focus-ring.active.gradient = {
                from = accent;
                to = accent;
                angle = 45;
                relative-to = "workspace-view";
                in' = "oklch longer hue";
              };

              shadow = {
                enable = true;
                color = palette.base.hex;
              };
            };

            overview.workspace-shadow.enable = false;

            window-rules =
              let
                niriLayout = niriSettings.layout;
                outerGap = niriLayout.gaps - niriLayout.border.width;
                # Move to the corner, do not focus, and give a border;
                notificationLike =
                  pos: rest:
                  rest
                  // {
                    focus-ring.enable = false;
                    border = {
                      enable = true;
                      inactive.color = accent;
                      active = niriLayout.focus-ring.active;
                    };
                    open-focused = false;
                    default-floating-position = {
                      x = outerGap;
                      y = outerGap;
                      relative-to = pos;
                    };
                  };
              in
              [
                {
                  # Lets clients without xdg-decoration protocols have transparent backgrounds
                  draw-border-with-background = false;
                  geometry-corner-radius =
                    let
                      radius = 12.0;
                    in
                    {
                      bottom-left = radius;
                      bottom-right = radius;
                      top-left = radius;
                      top-right = radius;
                    };
                  clip-to-geometry = true;
                  # Open clients to take up maximum space by default
                  open-maximized = true;
                }
                {
                  matches = [
                    { app-id = "^org\.pulseaudio\.pavucontrol$"; }
                    { app-id = "^com\.terminal\.nmtui$"; }
                  ];
                  open-floating = true;
                }
                # Hide password manager from screencasts
                {
                  matches = [ { app-id = "^org\.keepassxc\.KeePassXC$"; } ];
                  block-out-from = "screencast";
                }
                # Indicate screencasted windows
                {
                  matches = [
                    { is-window-cast-target = true; }
                  ];
                  focus-ring = {
                    active.color = palette.pink.hex;
                    inactive.color = palette.red.hex;
                  };
                  shadow.color = palette.red.hex;
                  tab-indicator = {
                    active.color = palette.pink.hex;
                    inactive.color = palette.red.hex;
                  };
                }
                # Steam notifications
                (notificationLike "bottom-right" {
                  matches = [
                    {
                      app-id = "steam";
                      title = "^notificationtoasts_\\d+_desktop$";
                    }
                  ];
                })
                # Dragon windows
                (notificationLike "top-left" {
                  matches = [
                    {
                      app-id = "^dragon-drop$";
                      title = "dragon";
                    }
                  ];
                })
                {
                  matches = [ { is-floating = true; } ];
                }
              ];

            layer-rules = [
              # Let various UI components have shadows
              {
                matches = [
                  { namespace = "^launcher$"; }
                  { namespace = "^notifications$"; }
                ];

                shadow.enable = true;
                # All of these are configured to use niri's corner radius like this anyways
                geometry-corner-radius = (builtins.elemAt niriSettings.window-rules 0).geometry-corner-radius;
              }
              # Put wallpapers in overview
              {
                matches = [
                  { namespace = "^$"; } # cava in windowtolayer
                  { namespace = "^wpaperd.*$"; }
                ];
                place-within-backdrop = true;
              }
            ];

            spawn-at-startup = [
              { command = [ (lib.getExe pkgs.xwayland-satellite) ]; }
              {
                command = [
                  "${lib.getExe pkgs.windowtolayer}"
                ]
                ++ terminal
                ++ [
                  "--background-opacity=0"
                  "-e"
                  "fish"
                  "-c"
                  "cava"
                ];
              }
              {
                command =
                  let
                    noHash = str: "0x${builtins.substring 1 (builtins.stringLength str) str}";
                  in
                  [
                    (lib.getExe pkgs.wayneko)
                    "--background-colour"
                    (noHash palette.crust.hex)
                    "--outline-colour"
                    (noHash accent)
                    "--layer"
                    "top"
                  ];
              }
              {
                command = [
                  "steam"
                  "-silent"
                ];
              }
            ];

            animations = {
              window-open = {
                kind.easing = {
                  curve = "linear";
                  duration-ms = 350;
                };
                custom-shader =
                  # glsl
                  ''
                    // Example: show the window as an expanding circle.
                    // Recommended setting: duration-ms 250
                    vec4 expanding_circle(vec3 coords_geo, vec3 size_geo) {
                        vec3 coords_tex = niri_geo_to_tex * coords_geo;
                        vec4 color = texture2D(niri_tex, coords_tex.st);

                        vec2 coords = (coords_geo.xy - vec2(0.5, 0.5)) * size_geo.xy * 2.0;
                        coords = coords / length(size_geo.xy);
                        float p = niri_clamped_progress;
                        if (p * p <= dot(coords, coords))
                            color = vec4(0.0);

                        return color;
                    }

                    // This is the function that you must define.
                    vec4 open_color(vec3 coords_geo, vec3 size_geo) {
                        return expanding_circle(coords_geo, size_geo);
                    }
                  '';
              };
              window-close = {
                kind.easing = {
                  curve = "linear";
                  duration-ms = 500;
                };
                custom-shader =
                  # glsl
                  ''
                    // Example: make the window 'fall down' with slight rotation.
                    vec4 fall_and_rotate(vec3 coords_geo, vec3 size_geo) {
                        // For this shader, set animation curve to linear for best results.

                        // Simulate an accelerated fall: square the (linear) progress.
                        float progress = niri_clamped_progress * niri_clamped_progress;

                        // Get our rotation pivot point coordinates at the bottom center of the window.
                        vec2 coords = (coords_geo.xy - vec2(0.5, 1.0)) * size_geo.xy;

                        // Move the window down to simulate a fall.
                        coords.y -= progress * 1440.0;

                        // Randomize rotation direction and maximum angle.
                        float random = (niri_random_seed - 0.5) / 2.0;
                        random = sign(random) - random;
                        float max_angle = 0.5 * random;

                        // Rotate the window around our pivot point.
                        float angle = progress * max_angle;
                        mat2 rotate = mat2(cos(angle), -sin(angle), sin(angle), cos(angle));
                        coords = rotate * coords;

                        // Transform the coordinates back.
                        coords_geo = vec3(coords / size_geo.xy + vec2(0.5, 1.0), 1.0);

                        // Sample the window texture.
                        vec3 coords_tex = niri_geo_to_tex * coords_geo;
                        vec4 color = texture2D(niri_tex, coords_tex.st);

                        // Multiply by alpha to fade out.
                        // return color * (1.0 - niri_clamped_progress);
                        return color;
                    }

                    // This is the function that you must define.
                    vec4 close_color(vec3 coords_geo, vec3 size_geo) {
                        return fall_and_rotate(coords_geo, size_geo);
                    }
                  '';
              };
              window-resize = {
                kind.easing = {
                  curve = "linear";
                  duration-ms = 250;
                };
                custom-shader =
                  # glsl
                  ''
                    // Example: crossfade between previous and next texture, stretched to the
                    // current geometry.
                    vec4 crossfade(vec3 coords_curr_geo, vec3 size_curr_geo) {
                        // Convert coordinates into the texture space for sampling.
                        vec3 coords_tex_prev = niri_geo_to_tex_prev * coords_curr_geo;
                        vec4 color_prev = texture2D(niri_tex_prev, coords_tex_prev.st);

                        // Convert coordinates into the texture space for sampling.
                        vec3 coords_tex_next = niri_geo_to_tex_next * coords_curr_geo;
                        vec4 color_next = texture2D(niri_tex_next, coords_tex_next.st);

                        vec4 color = mix(color_prev, color_next, niri_clamped_progress);
                        return color;
                    }

                    // This is the function that you must define.
                    vec4 resize_color(vec3 coords_curr_geo, vec3 size_curr_geo) {
                        return crossfade(coords_curr_geo, size_curr_geo);
                    }
                  '';
              };
            };

            binds =
              let
                inherit (config.lib.niri) actions;
                wpctl = args: {
                  allow-when-locked = true;
                  action.spawn = [ "wpctl" ] ++ args;
                };
                mouse = action: {
                  cooldown-ms = 150;
                  inherit action;
                };
                sh = actions.spawn "sh" "-c";
              in
              {
                "Mod+Shift+Slash".action = actions.show-hotkey-overlay;

                "Mod+T".action.spawn = terminal;
                "Mod+D".action = sh "pkill fuzzel || fuzzel";
                "Mod+B".action.spawn = launch "x-scheme-handler/https";
                "Mod+N".action.spawn = launch "inode/directory";
                "Super+Alt+L".action = actions.spawn "wlogout" "--show-binds";
                "Mod+Ctrl+C".action = sh "cliphist list | fuzzel --dmenu --prompt='Copy to Clipboard:' | wl-copy";

                # Volume keys mappings for PipeWire & WirePlumber.
                XF86AudioRaiseVolume = wpctl [
                  "set-volume"
                  "@DEFAULT_AUDIO_SINK@"
                  "0.05+"
                  "--limit"
                  "1"
                ];
                XF86AudioLowerVolume = wpctl [
                  "set-volume"
                  "@DEFAULT_AUDIO_SINK@"
                  "0.05-"
                  "--limit"
                  "1"
                ];
                XF86AudioMute = wpctl [
                  "set-mute"
                  "@DEFAULT_AUDIO_SINK@"
                  "toggle"
                ];
                XF86AudioMicMute = wpctl [
                  "set-mute"
                  "@DEFAULT_AUDIO_SOURCE@"
                  "toggle"
                ];
                XF86AudioPlay.action = actions.spawn (lib.getExe pkgs.playerctl) "play-pause";

                "Mod+Q".action = actions.close-window;

                "Mod+H".action = actions.focus-column-left-or-last;
                "Mod+J".action = actions.focus-window-down;
                "Mod+K".action = actions.focus-window-up;
                "Mod+L".action = actions.focus-column-right-or-first;

                "Mod+Ctrl+H".action = actions.move-column-left;
                "Mod+Ctrl+J".action = actions.move-window-down;
                "Mod+Ctrl+K".action = actions.move-window-up;
                "Mod+Ctrl+L".action = actions.move-column-right;

                "Mod+Home".action = actions.focus-column-first;
                "Mod+End".action = actions.focus-column-last;
                "Mod+Ctrl+Home".action = actions.move-column-to-first;
                "Mod+Ctrl+End".action = actions.move-column-to-last;

                "Mod+Page_Down".action = actions.focus-workspace-down;
                "Mod+Page_Up".action = actions.focus-workspace-up;
                "Mod+Ctrl+Page_Down".action = actions.move-column-to-workspace-down;
                "Mod+Ctrl+Page_Up".action = actions.move-column-to-workspace-up;

                "Mod+Shift+Page_Down".action = actions.move-workspace-down;
                "Mod+Shift+Page_Up".action = actions.move-workspace-up;

                "Mod+WheelScrollDown" = mouse actions.focus-workspace-down;
                "Mod+WheelScrollUp" = mouse actions.focus-workspace-up;
                "Mod+Ctrl+WheelScrollDown" = mouse actions.move-column-to-workspace-down;
                "Mod+Ctrl+WheelScrollUp" = mouse actions.move-column-to-workspace-up;

                "Mod+WheelScrollRight" = mouse actions.focus-column-right-or-first;
                "Mod+WheelScrollLeft" = mouse actions.focus-column-left-or-last;
                "Mod+Ctrl+WheelScrollRight" = mouse actions.move-column-right;
                "Mod+Ctrl+WheelScrollLeft" = mouse actions.move-column-left;

                # Usually scrolling up and down with Shift in applications results in
                # horizontal scrolling; these binds replicate that.
                "Mod+Shift+WheelScrollDown" = mouse actions.focus-column-right-or-first;
                "Mod+Shift+WheelScrollUp" = mouse actions.focus-column-left-or-last;
                "Mod+Ctrl+Shift+WheelScrollDown" = mouse actions.move-column-right;
                "Mod+Ctrl+Shift+WheelScrollUp" = mouse actions.move-column-left;

                # Switches focus between the current and the previous workspace.
                "Mod+Tab".action = actions.focus-workspace-previous;

                # The following binds move the focused window in and out of a column.
                # If the window is alone, they will consume it into the nearby column to the side.
                # If the window is already in a column, they will expel it out.
                "Mod+BracketLeft".action = actions.consume-or-expel-window-left;
                "Mod+BracketRight".action = actions.consume-or-expel-window-right;

                # Consume one window from the right to the bottom of the focused column.
                "Mod+Comma".action = actions.consume-window-into-column;
                # Expel the bottom window from the focused column to the right.
                "Mod+Period".action = actions.expel-window-from-column;

                "Mod+R".action = actions.switch-preset-column-width;
                "Mod+Shift+R".action = actions.switch-preset-window-height;
                "Mod+Ctrl+R".action = actions.reset-window-height;
                "Mod+F".action = actions.maximize-column;
                "Mod+Shift+F".action = actions.fullscreen-window;
                "Mod+C".action = actions.center-column;
                "Mod+W".action = actions.toggle-column-tabbed-display;
                "Mod+O".action = actions.toggle-overview;

                "Mod+Minus".action = actions.set-column-width "-10%";
                "Mod+Equal".action = actions.set-column-width "+10%";

                # Finer height adjustments when in column with other windows.
                "Mod+Shift+Minus".action = actions.set-window-height "-10%";
                "Mod+Shift+Equal".action = actions.set-window-height "+10%";

                # Move the focused window between the floating and the tiling layout.
                "Mod+V".action = actions.toggle-window-floating;
                "Mod+Shift+V".action = actions.switch-focus-between-floating-and-tiling;

                "Print".action = actions.screenshot;
                # TODO: wait for upstream fix, see sodiboo/niri-flake#922
                "Ctrl+Print".action.screenshot-screen = [ ];
                "Alt+Print".action = actions.screenshot-window;

                # The quit action will show a confirmation dialog to avoid accidental exits.
                "Mod+Shift+E".action = actions.quit;
                "Ctrl+Alt+Delete".action = actions.quit;

                "Mod+Shift+P".action = actions.power-off-monitors;
              }
              // lib.mergeAttrsList (
                lib.genList (
                  x:
                  let
                    workspace = x + 1;
                    key = toString (lib.trivial.mod workspace workspaces);
                  in
                  {
                    "Mod+${key}".action = actions.focus-workspace workspace;
                    # TODO: wait for upstream fix, see sodiboo/niri-flake#1018
                    "Mod+Ctrl+${key}".action.move-column-to-workspace = workspace;
                  }
                ) workspaces
              );
          };

        # For some reason, the Niri module does not provide this by default
        systemd.user.targets.tray = {
          Unit = {
            Description = "Home Manager System Tray";
            Requires = [ "graphical-session-pre.target" ];
          };
        };
      };
  };
}
