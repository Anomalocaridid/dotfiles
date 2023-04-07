{ config, ... }:

with config.lib;
with config.lib.stylix.scheme.withHashtag;
{
  programs.wezterm = {
    enable = true;
    # colorSchemes = (stylix.scheme inputs.base16-wezterm);
    colorSchemes = {
      base16 = rec {
        foreground = base05;
        background = base00;
        cursor_bg = foreground;
        cursor_border = foreground;
        cursor_fg = background;
        selection_bg = base02;
        selection_fg = base06;
        split = base01; # maybe tweak later
        scrollbar_thumb = base01; # maybe tweak later

        ansi = [
          base00 base08 base0B base0A
          base0D base0E base0C base05
        ];
        brights = [
          base03 base09 base01 base02
          base04 base06 base0F base07
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

    	font_size = ${builtins.toString config.stylix.fonts.sizes.terminal},
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

    	color_scheme = "base16",
    	colors = {
    		tab_bar =  {
    			background = "${base00}",

    			active_tab = {
    				bg_color = "${base01}",
    				fg_color = "${base05}",
    				intensity = "Bold",
    			},

    			inactive_tab = {
    				bg_color = "${base00}",
            fg_color = "${base05}",
    			},

    			inactive_tab_hover = {
    				bg_color = "${base00}",
    				fg_color = "${base0E}",    			
          },
    		},
    	},
    }
    '';
  };
}
