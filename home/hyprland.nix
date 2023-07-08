{ lib, pkgs, ... }: {
  # Additional dependencies
  home.packages = with pkgs; [
    mpvpaper # Live wallpaper
    hyprland-autoname-workspaces
  ];
  wayland.windowManager.hyprland = {
    enable = true;
    extraConfig =
      let
        # Switch workspaces with mainMod + [0-9]
        # Move active window to a workspace with mainMod + SHIFT + [0-9]
        workspaceSwitch = lib.strings.concatMapStrings
          (n:
            let
              key = (if workspace == "10" then "0" else workspace);
              workspace = toString n;
            in
            #hypr
            '' 
              bind = $mainMod, ${key}, workspace, ${workspace}
              bind = $mainMod SHIFT, ${key}, movetoworkspace, ${workspace}
            ''
          )
          (lib.lists.range 1 10);
        # Move focus with mainMod + direction keys
        # Swap active window with an adjacent window with mainMod + SHIFT + direction keys
        directionSwitch = lib.strings.concatMapStrings
          (key:
            let
              dir = (builtins.elemAt (lib.strings.stringToCharacters key) 1);
            in
            #hypr
            ''
              bind = $mainMod, ${key}, movefocus, ${dir}
              bind = $mainMod SHIFT, ${key}, swapwindow, ${dir}
            ''
          ) [ "$left" "$down" "$up" "$right" ];
        pink = "ea00d9";
        purple = "711c91";
        darkBlue = "000b1e";
        lightBlue = "133e7c";
        cyan = "0abdc6";
        green = "00ff00";
        orange = "f57800";
        red = "ff00ff";
      in
      #hypr
      ''
        # Execute your favorite apps at launch
        # TODO: steam
        # Set up live wallpaper
        # https://moewalls.com/landscape/synthwave-city-live-wallpaper/
        exec-once = mpvpaper -o "no-audio loop" HDMI-A-1 "/etc/nixos/assets/wallpaper.mp4"
        exec-once = eww open bar
        exec-once = hyprland-autoname-workspaces
        exec-once = webcord

        general {
            gaps_in = 5
            gaps_out = 20
            border_size = 2
            col.active_border = rgba(${pink}ee) rgba(${purple}ee) 45deg
            col.inactive_border = rgba(${darkBlue}aa) rgba(${lightBlue}aa) 45deg
            # col.group_border = rgba(${darkBlue}aa) rgba(${lightBlue}ee) 45deg
            # col.group_border_active = rgba(${cyan}ee) rgba(${green}ee) 45deg
            # col.group_border_locked = rgba(${darkBlue}aa) rgba(${lightBlue}ee) 45deg
            # col.group_border_locked_active = rgba(${orange}ee) rgba(${red}ee) 45deg

            layout = dwindle
        }

        decoration {
            # See https://wiki.hyprland.org/Configuring/Variables/ for more

            rounding = 10
            blur = true
            blur_size = 3
            blur_passes = 1
            blur_new_optimizations = true

            drop_shadow = true
            shadow_range = 4
            shadow_render_power = 3
            col.shadow = rgba(${darkBlue}ee)
        }

        animations {
            enabled = true

            # Some default animations, see https://wiki.hyprland.org/Configuring/Animations/ for more

            bezier = myBezier, 0.05, 0.9, 0.1, 1.05

            animation = windows, 1, 7, myBezier
            animation = windowsOut, 1, 7, default, popin 80%
            animation = border, 1, 10, default
            animation = borderangle, 1, 8, default
            animation = fade, 1, 7, default
            animation = workspaces, 1, 6, default
        }

        dwindle {
            # See https://wiki.hyprland.org/Configuring/Dwindle-Layout/ for more
            pseudotile = true # master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
            preserve_split = true # you probably want this
        }

        master {
            # See https://wiki.hyprland.org/Configuring/Master-Layout/ for more
            new_is_master = true
        }

        gestures {
            # See https://wiki.hyprland.org/Configuring/Variables/ for more
            workspace_swipe = false
        }

        misc {
          enable_swallow = true
          swallow_regex = ^(org.wezfurlong.wezterm)$
          # groupbar_text_color = rgb(${cyan})
        }
        
        # Window rules
        windowrulev2 = float, class:^(wlogout|pavucontrol|nmtui)$
        windowrulev2 = workspace 1, class:^(lutris)$
        windowrulev2 = workspace 2, class:^(nyxt)$
        windowrulev2 = workspace 3, class:^(nnn)$
        windowrulev2 = workspace 4 silent, class:^(WebCord)$
        windowrulev2 = workspace 5, title:^(Spotify)$
        # Inhibit idle on fullscreen programs where keyboard/mouse may not be used for a while
        windowrulev2 = idleinhibit fullscreen, class:^(FreeTube)$
        
        $mainMod = SUPER
        
        $shellExec = zsh -c
        $opener = handlr launch
        $term = $opener x-scheme-handler/terminal --
        # Launch nnn in a shell so it gets the necessary variables and a custom class
        # FIXME: Cannot open preview-tui if nnn is opened with this
        $fileMan = $term --class=nnn -- $shellExec "$opener inode/directory"

        # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
        bind = $mainMod, Return, exec, $term
        bind = $mainMod, Q, killactive,
        bind = $mainMod SHIFT, Q, exec, wlogout
        bind = $mainMod, N, execr, $fileMan
        bind = $mainMod, V, togglefloating,
        bind = $mainMod, R, exec, wofi --show drun
        bind = $mainMod, P, pseudo, # dwindle
        bind = $mainMod, S, togglesplit, # dwindle
        bind = $mainMod, D, exec, hyprctl keyword general:layout dwindle
        bind = $mainMod, M, exec, hyprctl keyword general:layout master
        bind = $mainMod, O, exec, nyxt
        # bind = $mainMod, G, togglegroup
        # bind = $mainMod SHIFT, G, lockgroups, toggle

        # Vim-style homerow direction keys
        $left = h
        $down = j
        $up = k
        $right = l

        # Scroll through existing workspaces with mainMod + scroll
        bind = $mainMod, mouse_down, workspace, e+1
        bind = $mainMod, mouse_up, workspace, e-1

        $LMB = mouse:272
        $RMB = mouse:275

        # Move/resize windows with mainMod + LMB/RMB and dragging
        bindm = $mainMod, $LMB, movewindow
        bindm = $mainMod, $RMB, resizewindow

        # Resize submap
        bind = $mainMod ALT,R,submap,resize
        submap = resize

        binde = ,$right,resizeactive,10 0
        binde = ,$left,resizeactive,-10 0
        binde = ,$up,resizeactive,0 -10
        binde = ,$down,resizeactive,0 10

        bind = ,escape,submap,reset 

        submap=reset
      '' + workspaceSwitch + directionSwitch;
  };

  xdg.configFile."hyprland-autoname-workspaces/config.toml".source =
    let
      tomlFormat = pkgs.formats.toml { };
      green = "#00FF00";
    in
    tomlFormat.generate "hyprland-autoname-workspaces-config" {
      version = "1.1.0";

      # TODO: Investigate if it would be possible to use eww literals as a replacement for inline pango
      format = rec {
        # Deduplicate icons if enable.
        # A superscripted counter will be added.
        dedup = true;
        dedup_inactive_fullscreen = true; # dedup more
        # window delimiter
        delim = " ";

        # available formatter:
        # {counter_sup} - superscripted count of clients on the workspace, and simple {counter}, {delim}
        # {icon}, {client}
        # workspace formatter
        workspace = "${workspace_empty}:{delim}{clients}"; # {id}, {delim} and {clients} are supported
        workspace_empty = "{name}"; # {id}, {delim} and {clients} are supported
        # client formatter
        client = "{icon}";
        client_active = "<span color='${green}'>${client}</span>";

        # deduplicate client formatter
        # client_fullscreen = "[{icon}]";
        # client_dup = "{client}{counter_sup}";
        # client_dup_fullscreen = "[{icon}]{delim}{icon}{counter_unfocused}";
        # client_dup_active = "*{icon}*{delim}{icon}{counter_unfocused}";
      };

      class = {
        # Add your icons mapping
        # use double quote the key and the value
        # take class name from 'hyprctl clients'
        # "DEFAULT" = " {class}: {title}";
        "DEFAULT" = "";
        "blueman-manager" = "";
        "[Ff]irefox" = "";
        "FreeTube" = "";
        "libreoffice" = "󰈙";
        "lutris" = "";
        "Minecraft" = "󰍳";
        "nyxt" = "󰖟";
        "org.keepassxc.KeePassXC" = "󰌋";
        "org.prismlauncher.PrismLauncher" = "󰍳";
        "org.pwmt.zathura" = "";
        "org.wezfurlong.wezterm" = "";
        "pavucontrol" = "󰕾";
        "steam" = "󰓓";
        "virt-manager" = "󰍺";
        "WebCord" = "󰙯";
        ".yubioath-flutter-wrapped_" = "󰌋";
        "nnn" = "";
      };

      # class_active = {};

      # initial_class = {};

      # initial_class_active = {};

      # regex captures support is supported
      # "emerge: (.+?/.+?)-.*" = "{match1}"

      title_in_class."^$" = {
        "(?i)spotify" = "";
      };

      # title_in_class_active = {};

      # title_in_initial_class = {};

      # initial_title = {};

      # initial_title_active = {};

      # initial_title_in_class = {};

      # initial_title_in_initial_class = {};

      # Add your applications that need to be exclude
      # The key is the class, the value is the title.
      # You can put an empty title to exclude based on
      # class name only, "" make the job.
      exclude = {
        "[Ss]team" = "(Friends List.*|^$)"; # will match all Steam window with null title (some popup)
      };

      # workspaces_name = {
      #   "1" = "一";
      #   "2" = "二";
      #   "3" = "三";
      #   "4" = "四";
      #   "5" = "五";
      #   "6" = "六";
      #   "7" = "七";
      #   "8" = "八";
      #   "9" = "九";
      #   "10" = "十";
      # };
    };
}

