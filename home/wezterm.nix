{ config, ... }: {
  programs.wezterm = {
    enable = true;
    extraConfig =
      let
        fonts = config.stylix.fonts;
      in
      # lua
      ''
        return {
        	hide_tab_bar_if_only_one_tab = true,
        	window_background_opacity = 0.90,
        	exit_behavior = "Close",
        	check_for_updates = false,
        	enable_wayland = true,
        	enable_kitty_graphics = true,
          window_close_confirmation = "NeverPrompt",

        	font_size = ${toString fonts.sizes.terminal},
        	font = wezterm.font {
        		family = "${fonts.monospace.name}",
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

        	color_scheme = "catppuccin-mocha",

          -- TODO:
        	--colors = {
        	--	tab_bar =  {
        	--		background = "#000b1e",

        	--		active_tab = {
        	--			bg_color = "#005faf",
        	--			fg_color = "#0abdc6",
        	--			intensity = "Bold",
        	--		},

        	--		inactive_tab = {
        	--			bg_color = "#000b1e",
          --      fg_color = "#0abdc6",
        	--		},

        	--		inactive_tab_hover = {
        	--			bg_color = "#000b1e",
        	--			fg_color = "#711c91",    			
          --    },
        	--	},
        	--},
        }
      '';
  };
}
