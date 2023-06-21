{ ... }: {
  programs.wezterm = {
    enable = true;
    colorSchemes = {
      cyberpunk-neon = rec {
        foreground = "#0abdc6";
        background = "#000b1e";
        cursor_bg = foreground;
        cursor_border = cursor_bg;
        cursor_fg = background;
        selection_bg = "#321959";
        selection_fg = "#d7d7d5";
        split = "#ea00d9";
        scrollbar_thumb = "#ea00d9";

        ansi = [
          "#123e7c"
          "#ff0000"
          "#d300c4"
          "#f57800"
          "#123e7c"
          "#711c91"
          "#0abdc6"
          "#d7d7d5"
        ];
        brights = [
          "#1c61c2"
          "#ff0000"
          "#d300c4"
          "#f57800"
          "#00ff00"
          "#711c91"
          "#0abdc6"
          "#d7d7d5"
        ];
      };
    };
    extraConfig =
      # lua
      ''
        return {
        	hide_tab_bar_if_only_one_tab = true,
        	window_background_opacity = 0.90,
        	bold_brightens_ansi_colors = false,
        	exit_behavior = "Close",
        	check_for_updates = false,
        	enable_wayland = true,
        	enable_kitty_graphics = true,
          window_close_confirmation = "NeverPrompt",

        	font_size = 11.0,
        	font = wezterm.font {
        		family = "FiraCode Nerd Font",
        		harfbuzz_features = {
        			"ss09", -- >>= <<= ||= |=
        			"cv25", -- .-
        			"cv26", -- :-
        			"cv32", -- .=
        			"cv27", -- []
        			"cv28", -- {. .}
        			"ss07", -- =~ !~
        		},
        	},

        	color_scheme = "cyberpunk-neon",
        	colors = {
        		tab_bar =  {
        			background = "#000b1e",

        			active_tab = {
        				bg_color = "#005faf",
        				fg_color = "#0abdc6",
        				intensity = "Bold",
        			},

        			inactive_tab = {
        				bg_color = "#000b1e",
                fg_color = "#0abdc6",
        			},

        			inactive_tab_hover = {
        				bg_color = "#000b1e",
        				fg_color = "#711c91",    			
              },
        		},
        	},
        }
      '';
  };
}
