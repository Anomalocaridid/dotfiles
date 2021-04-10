local wezterm = require 'wezterm';

return {
    font_size = 11.0,
    font = wezterm.font("FiraCode Nerd Font"),
    custom_block_glyphs = false,
	hide_tab_bar_if_only_one_tab = true,
	window_background_opacity = 0.90,
	bold_brightens_ansi_colors = false,
    
    colors = {
        foreground = "#0abdc6",
        background = "#000b1e",

        cursor_bg = "#0abdc6",
        cursor_fg = "#000b1e",

        ansi = {"#123e7c", "#ff0000", "#d300c4", "#f57800", "#123e7c", "#711c91", "#0abdc6", "#d7d7d5"},
        brights = {"#1c61c2", "#ff0000", "#d300c4", "#f57800", "#00ff00", "#711c91", "#0abdc6", "#d7d7d5"},

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

    keys = {
    	-- Horizontal Pane Split
        {key="\\", mods="SUPER", action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}},
        -- Vertical Pane Split
		{key="|", mods="SUPER", action=wezterm.action{SplitVertical={domain="CurrentPaneDomain"}}},

		-- Go one tab to the left
		{key="[", mods="SUPER", action=wezterm.action{ActivateTabRelative=-1}},
		-- Go one tab to the right
		{key="]", mods="SUPER", action=wezterm.action{ActivateTabRelative=1}},

		-- Close current pane
		{key="x", mods="SUPER", action=wezterm.action{CloseCurrentPane={confirm=false}}},
		-- Close current tab
		{key="x", mods="SUPER|SHIFT", action=wezterm.action{CloseCurrentTab={confirm=false}}},

    },
}
