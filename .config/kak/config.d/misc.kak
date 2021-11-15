# ~/.config/kak/config.d/misc.kak
# Kakoune miscellaneous settings

# Highlight matching characters
add-highlighter global/ show-matching

# Get rid of Clippy and don't set terminal window title
set-option global ui_options terminal_assistant=none terminal_set_title=false

# Tabs are 4 characters long
set-option global tabstop 4
set-option global indentwidth 4

# But Lisp, Haskell, and YAML have 2 character tabs
hook global WinSetOption filetype=(lisp|haskell|yaml) %{
	set-option window tabstop 2
	set-option window indentwidth 2
}

# Format shell scripts on save
# (No LSP server to format it)
hook global WinSetOption filetype=sh %{
	set-option window formatcmd "shfmt -ci -sr"
	hook buffer BufWritePre .* format
}

# Format dhall files on save
# (No LSP server to format it)
hook global WinSetOption filetype=dhall %{
	set-option window formatcmd "dhall format"
	hook buffer BufWritePre .* format
}
