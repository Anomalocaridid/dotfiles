local wezterm = require 'wezterm';

return {
	font_size = 11.0,
	font = wezterm.font("FiraCode Nerd Font"),
	hide_tab_bar_if_only_one_tab = true,
	window_background_opacity = 0.90,
	bold_brightens_ansi_colors = false,
	exit_behavior = "Close",
	check_for_updates = false,
	enable_wayland = true,

	color_scheme = "Cyberpunk Neon",

	colors = {
		tab_bar = {
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

	-- keys = {
	-- },
}
