{ config, lib, pkgs, ... }: {
  # Additional dependencies
  home.packages = with pkgs; [
    mpvpaper # Live wallpaper
    hyprland-autoname-workspaces
    custom.screenshot
  ];

  wayland.windowManager.hyprland = {
    enable = true;
    settings =
      let
        # Temporary workaround because home manager hyprland module does not handle importing well
        theme = pkgs.custom.parseTheme {
          name = "hyprland";
          themeFile = pkgs.fetchFromGitHub
            {
              owner = "catppuccin";
              repo = "hyprland";
              rev = "99a88fd21fac270bd999d4a26cf0f4a4222c58be";
              hash = "sha256-07B5QmQmsUKYf38oWU3+2C6KO4JvinuTwmW1Pfk8CT8=";
            } + "/themes/${config.catppuccin.flavour}.conf";
          keyField = 1;
          valueField = 3;
        };
        accent = "\$${config.catppuccin.accent}";
      in
      #hypr
      theme //
      {
        # Execute your favorite apps at launch
        exec-once = [
          # Set up live wallpaper
          # https://moewalls.com/pixel-art/cyberpunk-rain-city-pixel-live-wallpaper/
          "mpvpaper -o 'no-audio loop' HDMI-A-1 '/etc/nixos/assets/wallpaper.mp4'"
          "eww open bar"
          "hyprland-autoname-workspaces"
          # "armcord"
          "steam -silent"
        ];

        general = {
          gaps_in = 5;
          gaps_out = 20;
          border_size = 2;
          "col.active_border" = "0xee$lavenderAlpha 0xee${accent}Alpha 45deg";
          "col.inactive_border" = "0xaa$overlay0Alpha 0xaa$mantleAlpha 45deg";
          "col.group_border" = "0xaa$overlay0Alpha 0xaa$yellowAlpha 45deg";
          "col.group_border_active" = "0xee$yellowAlpha";
          "col.group_border_locked" = "0xaa$overlay0Alpha 0xaa$redAlpha 45deg";
          "col.group_border_locked_active" = "0xee$yellowAlpha 0xee$redAlpha 45deg";

          layout = "dwindle";
          cursor_inactive_timeout = 60;
        };

        decoration = {
          # See https://wiki.hyprland.org/Configuring/Variables/ for more
          rounding = 10;

          blur = {
            enabled = true;
            size = 3;
            passes = 1;
          };

          drop_shadow = true;
          shadow_range = 4;
          shadow_render_power = 3;
          "col.shadow" = "$base";
        };

        animations = {
          enabled = true;

          # Some default animations, see https://wiki.hyprland.org/Configuring/Animations/ for more
          bezier = "myBezier, 0.05, 0.9, 0.1, 1.05";

          animation = [
            "windows, 1, 7, myBezier"
            "windowsOut, 1, 7, default, popin 80%"
            "border, 1, 10, default"
            "borderangle, 1, 8, default"
            "fade, 1, 7, default"
            "workspaces, 1, 6, default"
          ];
        };

        dwindle = {
          # See https://wiki.hyprland.org/Configuring/Dwindle-Layout/ for more
          pseudotile = true; # master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
          preserve_split = true; # you probably want this
        };

        # See https://wiki.hyprland.org/Configuring/Master-Layout/ for more
        master.new_is_master = true;

        # See https://wiki.hyprland.org/Configuring/Variables/ for more
        gestures.workspace_swipe = false;

        misc = {
          enable_swallow = true;
          swallow_regex = "^(org.wezfurlong.wezterm)$";
          groupbar_text_color = "$text";
        };

        # Window rules
        windowrulev2 = [
          "float, class:^(wlogout|pavucontrol|nmtui)$"
          "workspace 1, class:^(lutris)$"
          "workspace 2, class:^(nyxt)$"
          "workspace 3, class:^(nnn)$"
          "workspace 4 silent, class:^(ArmCord)$"
          "workspace 5, title:^(Spotify)$"
          # Inhibit idle on fullscreen programs where keyboard/mouse may not be used for a while
          "idleinhibit fullscreen, class:^(FreeTube)$"
          # Required for locking script
          "fullscreen, class:^(lockman)$"
        ];

        "$mainMod" = "SUPER";

        "$opener" = "handlr launch";
        "$term" = "$opener x-scheme-handler/terminal --";

        # Vim-style homerow direction keys
        "$left" = "h";
        "$down" = "j";
        "$up" = "k";
        "$right" = "l";

        # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
        bind = lib.flatten [
          "$mainMod, Return, exec, $term"
          "$mainMod, Q, killactive,"
          "$mainMod SHIFT, Q, exec, wlogout"
          # Launch nnn in a shell so it gets the necessary variables and a custom class
          # TODO: Figure out how to not need the shell
          "$mainMod, N, exec, $term --class=nnn -- zsh -c '$opener inode/directory'"
          "$mainMod, V, togglefloating,"
          "$mainMod, R, exec, wofi --show drun"
          "$mainMod, P, pseudo, # dwindle"
          "$mainMod, S, togglesplit, # dwindle"
          "$mainMod, D, exec, hyprctl keyword general:layout dwindle"
          "$mainMod, M, exec, hyprctl keyword general:layout master"
          "$mainMod, O, exec, nyxt"
          "$mainMod, G, togglegroup"
          "$mainMod SHIFT, G, lockactivegroup, toggle"
          "$mainMod, bracketleft, changegroupactive, b"
          "$mainMod, bracketright, changegroupactive, f"
          ", Print, exec, screenshot.sh"

          # Move focus with mainMod + direction keys
          # Move active window with mainMod + SHIFT + direction keys
          (builtins.map
            (key:
              let
                dir = (builtins.elemAt (lib.strings.stringToCharacters key) 1);
              in
              [
                "$mainMod, ${key}, movefocus, ${dir}"
                "$mainMod SHIFT, ${key}, movewindoworgroup, ${dir}"
              ]
            )
            [ "$left" "$down" "$up" "$right" ])

          # Switch workspaces with mainMod + [0-9]
          # Move active window to a workspace with mainMod + SHIFT + [0-9]
          (builtins.genList
            (x:
              let
                ws = x + 1;
                key = toString (lib.trivial.mod ws 10);
              in
              [
                "$mainMod, ${key}, workspace, ${toString ws}"
                "$mainMod SHIFT, ${key}, movetoworkspace, ${toString ws}"
              ]
            )
            10)

          # Scroll through existing workspaces with mainMod + scroll
          "$mainMod, mouse_down, workspace, e+1"
          "$mainMod, mouse_up, workspace, e-1"
        ];

        "$LMB" = "mouse:272";
        "$RMB" = "mouse:275";

        bindm = [
          # Move/resize windows with mainMod + LMB/RMB and dragging
          "$mainMod, $LMB, movewindow"
          "$mainMod, $RMB, resizewindow"
        ];

      };
    extraConfig = #hypr
      ''
        # Resize submap
        bind = $mainMod ALT,R,submap,resize
        submap = resize

        binde = ,$right,resizeactive,10 0
        binde = ,$left,resizeactive,-10 0
        binde = ,$up,resizeactive,0 -10
        binde = ,$down,resizeactive,0 10

        bind = ,escape,submap,reset

        submap = reset
      '';
  };

  xdg.configFile = {
    "hyprland-autoname-workspaces/config.toml".source =
      let
        tomlFormat = pkgs.formats.toml { };
        palette = pkgs.custom.catppuccin-palette.${config.catppuccin.flavour};
      in
      tomlFormat.generate "hyprland-autoname-workspaces-config" {
        version = pkgs.hyprland-autoname-workspaces.version;

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
          client_active = "<span color='#${palette.green.hex}'>${client}</span>";

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
          "blueman-manager" = "";
          "DEFAULT" = "";
          "[Ff]irefox" = "";
          "FreeTube" = "";
          "libreoffice" = "󰈙";
          "lutris" = "";
          "Minecraft" = "󰍳";
          "nnn" = "";
          "nyxt" = "󰖟";
          "org.keepassxc.KeePassXC" = "󰌋";
          "org.prismlauncher.PrismLauncher" = "󰍳";
          "org.pwmt.zathura" = "";
          "org.wezfurlong.wezterm" = "";
          "pavucontrol" = "󰕾";
          "Py[Ss]ol" = "󰣎";
          "steam" = "󰓓";
          "virt-manager" = "󰍺";
          "ArmCord" = "󰙯";
          ".yubioath-flutter-wrapped_" = "󰌋";
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
          "" = "^$"; # Hide XWayland windows that remain after closing
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

    # For screenshot.sh
    "swappy/config".source =
      let
        iniFormat = pkgs.formats.ini { };
        fonts = config.stylix.fonts;
      in
      iniFormat.generate "swappy-config" {
        Default = {
          save_dir = "$HOME/Pictures/Screenshots";
          text_size = fonts.sizes.applications;
          text_font = fonts.sansSerif.name;
        };
      };
  };
}
