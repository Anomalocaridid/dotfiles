{ lib, ... }:
{
  programs.starship = {
    enable = true;
    enableTransience = true;
    settings =
      let
        langs = {
          c = "";
          cmake = "";
          haskell = "";
          java = "";
          julia = "";
          lua = "󰢱";
          nodejs = "󰌞";
          ocaml = "";
          python = "";
          rust = "";
        };
        mkLangSeg = name: symbol: {
          "${name}" = {
            symbol = symbol;
            style = "fg:green bg:surface0";
            format = "[ $symbol ($version)]($style)";
          };
        };
        langNames = builtins.attrNames langs;
        langSegments = builtins.foldl' (x: y: x // y) { } (
          lib.lists.zipListsWith mkLangSeg langNames (builtins.attrValues langs)
        );
      in
      langSegments
      // {
        format = lib.concatStrings (
          [
            "[](surface0)"
            "$username"
            "$hostname"
            "[](fg:surface0 bg:surface1)"
            "$directory"
            "[](fg:surface1 bg:surface2)"
            "$git_branch"
            "$git_status"
            "[](fg:surface2 bg:surface0)"
          ]
          ++ (map (x: "$" + x) langNames)
          ++ [
            "[](surface0)"
            "$fill"
            "[](surface0)"
            "$cmd_duration"
            "[](fg:surface1 bg:surface0)"
            "$jobs"
            "[](fg:surface2 bg:surface1)"
            "$status"
            "[](fg:surface0 bg:surface2)"
            "$localip"
            "[](surface0)"
            "$line_break"
            "$character"
          ]
        );

        # Bottom right only
        right_format = lib.concatStrings [
          "[](surface2)"
          "$memory_usage"
          "[](fg:surface1 bg:surface2)"
          "$time"
          "[](fg:surface1)"
        ];

        # Upper left
        username = {
          show_always = true;
          style_user = "fg:flamingo bg:surface0";
          style_root = "fg:red bg:surface0";
          format = "[ $user]($style)";
        };

        hostname = {
          ssh_only = false;
          style = "fg:pink bg:surface0";
          format = "[@$hostname ]($style)";
        };

        directory = {
          style = "fg:maroon  bg:surface1";
          read_only = "";
          read_only_style = "fg:red bg:surface1";
          format = "[  $path]($style)[$read_only ]($read_only_style)";
          truncation_length = 3;
          truncation_symbol = "…/";
          substitutions = {
            "Documents" = "󰈙 ";
            "Downloads" = " ";
            "Music" = " ";
            "Pictures" = " ";
          };
        };

        git_branch = {
          symbol = "";
          style = "fg:peach bg:surface2";
          format = "[ $symbol $branch ]($style)";
        };

        git_status = {
          conflicted = "$count";
          ahead = "$count";
          behind = "$count";
          diverged = "󱒓$behind_count/$ahead_count";
          untracked = "$count";
          stashed = "󱉙$count";
          modified = "$count";
          staged = "󰩍$count";
          renamed = "󰪹$count";
          deleted = "$count";
          style = "fg:yellow bg:surface2";
          format = "[($all_status$ahead_behind )]($style)";
        };

        fill.symbol = " ";

        # Upper right
        cmd_duration = {
          min_time = 10000;
          style = "bold fg:teal bg:surface0";
          format = "[ $duration  ]($style)";
          show_milliseconds = true;
        };

        jobs = {
          style = "bold fg:sky bg:surface1";
          symbol = "";
          format = "[ $number$symbol ]($style)";
        };

        status = {
          disabled = false;
          style = "bold fg:sapphire bg:surface2";
          symbol = "✘";
          not_executable_symbol = "󰂭";
          not_found_symbol = "";
          sigint_symbol = "";
          signal_symbol = "";
          map_symbol = true;
          format = "[ $common_meaning $status $symbol ]($style)";
        };

        localip = {
          disabled = false;
          ssh_only = false;
          style = "fg:blue bg:surface0";
          format = "[ $localipv4 󰛳 ]($style)";
        };

        # Lower left
        # Colors chosed to match modes on helix theme
        character = {
          vimcmd_symbol = "[❮](bold lavender)";
          # These below do not seem to work for some reason
          vimcmd_replace_one_symbol = "[❮](bold green)";
          vimcmd_replace_symbol = "[❮](bold green)";
          vimcmd_visual_symbol = "[❮](bold flamingo)";
        };

        # Lower right
        memory_usage = {
          disabled = false;
          threshold = 0;
          symbol = "󰍛";
          style = "fg:lavender bg:surface2";
          format = "[ $ram_pct $swap_pct $symbol ]($style)";
        };

        time = {
          disabled = false;
          time_format = "%T"; # Hour:Minute:Second Format
          style = "fg:mauve bg:surface1";
          format = "[ $time  ]($style)";
        };
      };
  };
}
