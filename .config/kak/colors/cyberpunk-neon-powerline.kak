hook global ModuleLoaded powerline %{ require-module powerline_cyberpunk_neon }

provide-module powerline_cyberpunk_neon %§
set-option -add global powerline_themes "cyberpunk-neon"

define-command -hidden powerline-theme-cyberpunk-neon %{
    # bufname
    declare-option -hidden str powerline_color00 "rgb:ff00ff" # fg
    declare-option -hidden str powerline_color03 "rgb:123e7c" # bg

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

§

