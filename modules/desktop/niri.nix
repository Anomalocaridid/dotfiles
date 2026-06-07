{ config, inputs, ... }:
let
  inherit (config.flake.meta) wm;
in
{
  # Provides a binary cache, so do not follow inputs
  flake-file.inputs.niri-nix.url = "git+https://codeberg.org/BANanaD3V/niri-nix";

  # Needed for various programs to make theming consistent
  flake.meta.wm = rec {
    # Needs to be a float for niri, but not for other things that need it
    windowCornerRadius = 12;
    # Currently set to niri's default border width
    borderWidth = 4;
    # Currently set to niri's default gap width
    gapWidth = 16;
    gapMinusBorder = gapWidth - borderWidth;
    mouseCooldownMs = 150;
    workspaces = 10;
  };

  unify.modules.general = {
    nixos = {
      imports = [ inputs.niri-nix.nixosModules.default ];
      programs.niri.enable = true;
      # Tell electron apps to use Wayland
      environment.sessionVariables.NIXOS_OZONE_WL = "1";
    };

    home =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        imports = [ inputs.niri-nix.homeModules.default ];
        home.packages = with pkgs; [ xwayland-satellite ];

        xdg.autostart = {
          enable = true;
          readOnly = true;
        };

        wayland.windowManager.niri = {
          enable = true;
          settings =
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
              # radius = wm.windowCornerRadius + 0.0;
              geometry-corner-radius = wm.windowCornerRadius;
              active-gradient._props = {
                from = accent;
                to = accent;
                angle = 45;
                relative-to = "workspace-view";
                "in" = "oklch longer hue";
              };
            in
            {
              # Prefer no client-side decorations to make borders and rounded corners a bit more consistent
              prefer-no-csd = true;

              hotkey-overlay = {
                skip-at-startup = true;
                hide-not-bound = true;
              };

              layout = {
                gaps = wm.gapWidth;

                always-center-single-column = true;

                # Show backdrop as wallpaper
                background-color = "transparent";

                border = {
                  off = [ ];
                  width = wm.borderWidth;
                };

                focus-ring = {
                  inherit active-gradient;
                  width = wm.borderWidth;
                };

                tab-indicator.width = wm.borderWidth;

                shadow = {
                  on = [ ];
                  color = palette.base.hex;
                };
              };

              overview.workspace-shadow.off = [ ];

              window-rule =
                let
                  # Move to the corner, do not focus, and give a border;
                  notificationLike =
                    pos: rest:
                    rest
                    // {
                      focus-ring.off = [ ];
                      border = {
                        inherit active-gradient;
                        on = [ ];
                        inactive-color = accent;
                      };
                      open-focused = false;
                      default-floating-position._props = {
                        x = wm.gapMinusBorder;
                        y = wm.gapMinusBorder;
                        relative-to = pos;
                      };
                    };
                in
                [
                  {
                    inherit geometry-corner-radius;
                    # Lets clients without xdg-decoration protocols have transparent backgrounds
                    draw-border-with-background = false;
                    clip-to-geometry = true;
                    # Open clients to take up maximum space by default
                    open-maximized = true;
                  }
                  {
                    match = [
                      # Audio control panel
                      { _props.app-id = "^org\\.pulseaudio\\.pavucontrol$"; }
                      # Terminal network manager
                      { _props.app-id = "^com\\.terminal\\.nmtui$"; }
                      # Script dialog program
                      { _props.app-id = "^yad$"; }
                    ];
                    open-floating = true;
                  }
                  # Non-interactive script dialogues
                  {
                    # Used in sioyek OCRmyPDF script
                    match._props.app-id = "^com\\.terminal\\.ocrmypdf$";
                    open-floating = true;
                    open-focused = false;
                  }
                  # Hide password manager from screencasts
                  {
                    match._props.app-id = "^org\\.keepassxc\\.KeePassXC$";
                    block-out-from = "screencast";
                  }
                  # Indicate screencasted windows
                  {
                    match._props.is-window-cast-target = true;
                    focus-ring = {
                      active-color = palette.pink.hex;
                      inactive-color = palette.red.hex;
                    };
                    shadow.color = palette.red.hex;
                    tab-indicator = {
                      active-color = palette.pink.hex;
                      inactive-color = palette.red.hex;
                    };
                  }
                  # Steam notifications
                  (notificationLike "bottom-right" {
                    match._props = {
                      app-id = "steam";
                      title = "^notificationtoasts_\\d+_desktop$";
                    };
                  })
                  # Dragon windows
                  (notificationLike "top-left" {
                    match._props = {
                      app-id = "^dragon-drop$";
                      title = "dragon";
                    };
                  })
                ];

              layer-rule._children = [
                # Let various UI components have shadows
                {
                  inherit geometry-corner-radius;

                  match = [
                    { _props.namespace = "^launcher$"; }
                    { _props.namespace = "^notifications$"; }
                  ];

                  shadow.on = [ ];
                }
                # Put wallpapers in overview
                {
                  match = [
                    { _props.namespace = "^$"; } # cava in windowtolayer
                    { _props.namespace = "^wpaperd.*$"; }
                  ];
                  place-within-backdrop = true;
                }
              ];

              animations = {
                window-open = {
                  curve = "linear";
                  duration-ms = 350;
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
                  curve = "linear";
                  duration-ms = 500;
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
                  curve = "linear";
                  duration-ms = 250;
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
                  wpctl = args: {
                    _props.allow-when-locked = true;
                    spawn = [ "wpctl" ] ++ args;
                  };
                  launchBind = mime: title: {
                    _props.hotkey-overlay-title = "Launch ${title}";
                    spawn = launch mime;
                  };
                in
                {
                  "Mod+Shift+Slash".show-hotkey-overlay = [ ];

                  "Mod+T" = {
                    _props.hotkey-overlay-title = "Launch Terminal";
                    spawn = terminal;
                  };
                  "Mod+D" = {
                    _props.hotkey-overlay-title = "Open App Launcher";
                    spawn-sh = "pkill fuzzel || fuzzel";
                  };
                  "Mod+B" = launchBind "x-scheme-handler/https" "Web Browser";
                  "Mod+N" = launchBind "inode/directory" "File Manager";
                  "Super+Alt+L" = {
                    _props.hotkey-overlay-title = "Shut Down";
                    spawn = [
                      "wlogout"
                      "--show-binds"
                    ];
                  };
                  "Mod+Ctrl+C" = {
                    _props.hotkey-overlay-title = "View Clipboard History";
                    spawn-sh = "cliphist list | fuzzel --dmenu --prompt='Copy to Clipboard:' | wl-copy";
                  };
                  "Mod+S" = {
                    _props.hotkey-overlay-title = "Toggle Blue Light Filter";
                    spawn = [
                      (lib.getExe pkgs.sunsetr)
                      "preset"
                      "day"
                    ];
                  };

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
                  XF86AudioPlay.spawn = [
                    (lib.getExe pkgs.playerctl)
                    "play-pause"
                  ];

                  "Mod+Q".close-window = [ ];

                  "Mod+H".focus-column-left-or-last = [ ];
                  "Mod+J".focus-window-down = [ ];
                  "Mod+K".focus-window-up = [ ];
                  "Mod+L".focus-column-right-or-first = [ ];

                  "Mod+Ctrl+H".move-column-left = [ ];
                  "Mod+Ctrl+J".move-window-down = [ ];
                  "Mod+Ctrl+K".move-window-up = [ ];
                  "Mod+Ctrl+L".move-column-right = [ ];

                  "Mod+Home".focus-column-first = [ ];
                  "Mod+End".focus-column-last = [ ];
                  "Mod+Ctrl+Home".move-column-to-first = [ ];
                  "Mod+Ctrl+End".move-column-to-last = [ ];

                  "Mod+Page_Down".focus-workspace-down = [ ];
                  "Mod+Page_Up".focus-workspace-up = [ ];
                  "Mod+Ctrl+Page_Down".move-column-to-workspace-down = [ ];
                  "Mod+Ctrl+Page_Up".move-column-to-workspace-up = [ ];

                  "Mod+Shift+Page_Down".move-workspace-down = [ ];
                  "Mod+Shift+Page_Up".move-workspace-up = [ ];

                  "Mod+WheelScrollDown" = {
                    _props.cooldown-ms = wm.mouseCooldownMs;
                    focus-workspace-down = [ ];
                  };
                  "Mod+WheelScrollUp" = {
                    _props.cooldown-ms = wm.mouseCooldownMs;
                    focus-workspace-up = [ ];
                  };
                  "Mod+Ctrl+WheelScrollDown" = {
                    _props.cooldown-ms = wm.mouseCooldownMs;
                    move-column-to-workspace-down = [ ];
                  };
                  "Mod+Ctrl+WheelScrollUp" = {
                    _props.cooldown-ms = wm.mouseCooldownMs;
                    move-column-to-workspace-up = [ ];
                  };

                  "Mod+WheelScrollRight" = {
                    _props.cooldown-ms = wm.mouseCooldownMs;
                    focus-column-right-or-first = [ ];
                  };
                  "Mod+WheelScrollLeft" = {
                    _props.cooldown-ms = wm.mouseCooldownMs;
                    focus-column-left-or-last = [ ];
                  };
                  "Mod+Ctrl+WheelScrollRight" = {
                    _props.cooldown-ms = wm.mouseCooldownMs;
                    move-column-right = [ ];
                  };
                  "Mod+Ctrl+WheelScrollLeft" = {
                    _props.cooldown-ms = wm.mouseCooldownMs;
                    move-column-left = [ ];
                  };

                  # Usually scrolling up and down with Shift in applications results in
                  # horizontal scrolling; these binds replicate that.
                  "Mod+Shift+WheelScrollDown" = {
                    _props.cooldown-ms = wm.mouseCooldownMs;
                    focus-column-right-or-first = [ ];
                  };
                  "Mod+Shift+WheelScrollUp" = {
                    _props.cooldown-ms = wm.mouseCooldownMs;
                    focus-column-left-or-last = [ ];
                  };
                  "Mod+Ctrl+Shift+WheelScrollDown" = {
                    _props.cooldown-ms = wm.mouseCooldownMs;
                    move-column-right = [ ];
                  };
                  "Mod+Ctrl+Shift+WheelScrollUp" = {
                    _props.cooldown-ms = wm.mouseCooldownMs;
                    move-column-left = [ ];
                  };

                  # Switches focus between the current and the previous workspace.
                  "Mod+Tab".focus-workspace-previous = [ ];

                  # The following binds move the focused window in and out of a column.
                  # If the window is alone, they will consume it into the nearby column to the side.
                  # If the window is already in a column, they will expel it out.
                  "Mod+BracketLeft".consume-or-expel-window-left = [ ];
                  "Mod+BracketRight".consume-or-expel-window-right = [ ];

                  # Consume one window from the right to the bottom of the focused column.
                  "Mod+Comma".consume-window-into-column = [ ];
                  # Expel the bottom window from the focused column to the right.
                  "Mod+Period".expel-window-from-column = [ ];

                  "Mod+R".switch-preset-column-width = [ ];
                  "Mod+Shift+R".switch-preset-window-height = [ ];
                  "Mod+Ctrl+R".reset-window-height = [ ];
                  "Mod+F".maximize-column = [ ];
                  "Mod+Shift+F".fullscreen-window = [ ];
                  "Mod+C".center-column = [ ];
                  "Mod+W".toggle-column-tabbed-display = [ ];
                  "Mod+O".toggle-overview = [ ];

                  "Mod+Minus".set-column-width = "-10%";
                  "Mod+Equal".set-column-width = "+10%";

                  # Finer height adjustments when in column with other windows.
                  "Mod+Shift+Minus".set-window-height = "-10%";
                  "Mod+Shift+Equal".set-window-height = "+10%";

                  # Move the focused window between the floating and the tiling layout.
                  "Mod+V".toggle-window-floating = [ ];
                  "Mod+Shift+V".switch-focus-between-floating-and-tiling = [ ];

                  "Print".screenshot = [ ];
                  "Ctrl+Print".screenshot-screen = [ ];
                  "Alt+Print".screenshot-window = [ ];

                  # The quit action will show a confirmation dialog to avoid accidental exits.
                  "Mod+Shift+E".quit = [ ];
                  "Ctrl+Alt+Delete".quit = [ ];

                  "Mod+Shift+P".power-off-monitors = [ ];
                }
                // lib.mergeAttrsList (
                  lib.genList (
                    x:
                    let
                      workspace = x + 1;
                      key = toString (lib.trivial.mod workspace wm.workspaces);
                    in
                    {
                      "Mod+${key}".focus-workspace = workspace;
                      "Mod+Ctrl+${key}".move-column-to-workspace = workspace;
                    }
                  ) wm.workspaces
                );
            };
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
