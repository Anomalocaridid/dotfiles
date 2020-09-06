# Based on Roboron3042's Cyberpunk Neon colorscheme
# https://github.com/Roboron3042/Cyberpunk-Neon/

# code

face global value rgb:f57800,default
face global type rgb:ea00d9,default+b
face global identifier rgb:ea00d9,default
face global string rgb:f57800,default
face global error rgb:ff0000,rgb:000000+bu
face global keyword rgb:ea00d9,default+b
face global operator rgb:f57800,default
face global attribute rgb:ea00d9,default
face global comment rgb:133e7c,default
face global meta rgb:f57800,default

# text

face global title rgb:ea00d9,default+u
face global header default,default
face global bold rgb:00ff00,default+b
face global italic rgb:00ff00,default+i
face global mono rgb:333333,rgb:dedede
face global block rgb:333333,rgb:dedede
face global link rgb:ea00d9,default+u
face global bullet default,default
face global list default,default

# kakoune UI

face global Default rgb:0abdc6,rgb:000b1f
face global PrimarySelection rgb:d7d7d5,rgb:711c91
face global SecondarySelection rgb:0abdc6,rgb:133e7c
face global PrimaryCursor rgb:000b1f,rgb:0abdc6+b
face global SecondaryCursor rgb:272935,rgb:f8f8f2+b
face global MatchingChar default,default+u
face global Search default,default+u
face global Whitespace rgb:133e7c,default+b
face global BufferPadding rgb:133e7c,default
face global LineNumbers default,default
face global LineNumberCursor default,rgb:09254b+b
face global MenuForeground rgb:0abdc6,rgb:711c91
face global MenuBackground rgb:0abdc6,rgb:091833
face global MenuInfo rgb:d7d7d5,rgb:ea00d9
face global Information rgb:d7d7d5,rgb:ea00d9
face global Error rgb:ff0000,default+bu
face global StatusLine rgb:00ffff,rgb:000b1f
face global StatusLineMode rgb:f57600,rgb:000b1f
face global StatusLineInfo rgb:d100c3,rgb:000b1f
face global StatusLineValue rgb:00ff00,rgb:000b1f
face global StatusCursor rgb:000b1f,rgb:0abdc6
face global Prompt rgb:f57600,rgb:000b1f

# kak-crosshairs integration
face global crosshairs_line default,rgb:09254D+b
face global crosshairs_column default,rgb:09254D+b

# kakoune-roguelight integration
face global RogueLightBackground rgb:000b1e,default

# powerline.kak integration
hook global ModuleLoaded powerline %{ require-module powerline_cyberpunk_neon }

provide-module powerline_cyberpunk_neon %{
	set-option -add global powerline_themes "cyberpunk-neon"
	define-command -hidden powerline-theme-cyberpunk-neon %{
    	# bufname
    	declare-option -hidden str powerline_color00 "rgb:ff00ff" # fg
    	declare-option -hidden str powerline_color03 "rgb:133e7c" # bg

    	# position
    	declare-option -hidden str powerline_color05 "rgb:8700af" # fg
    	declare-option -hidden str powerline_color01 "rgb:00005f" # bg

    	# git
    	declare-option -hidden str powerline_color02 "rgb:5fd700" # fg
    	declare-option -hidden str powerline_color04 "rgb:8700af" # bg

    	# line-column
    	declare-option -hidden str powerline_color06 "rgb:00d7d7" # fg
    	declare-option -hidden str powerline_color09 "rgb:005faf" # bg

    	# mode-info
    	declare-option -hidden str powerline_color07 "default" # fg
		declare-option -hidden str powerline_color08 "000b1e" # bg

    	# filetype
    	declare-option -hidden str powerline_color10 "rgb:8700af" # fg
    	declare-option -hidden str powerline_color11 "rgb:00d7d7" # bg

    	# client
    	declare-option -hidden str powerline_color12 "rgb:ff00ff"  # fg
    	declare-option -hidden str powerline_color13 "rgb:eeeeee"  # bg

    	# session
    	declare-option -hidden str powerline_color15 "rgb:00d7d7" # fg
    	declare-option -hidden str powerline_color14 "rgb:005faf" # bg

    	declare-option -hidden str powerline_next_bg "rgb:000b1e"
    	declare-option -hidden str powerline_base_bg "rgb:000b1e"
	}
}
