# ~/.config/zsh/config.d/commands.zsh
# Custom commands for zsh.

# Aliases alternative programs to commonly used commands
alias ls="exa --color=always --group-directories-first --icons"
alias du="dust"
alias find="fd"
alias grep="rg"
alias filegrep="rg --files | rg"
alias cat="bat --paging=never"
alias less="bat --paging=always"
alias man="batman"
alias diff="batdiff"
batthemes() {
	bat --list-themes | fzf --preview="bat --theme={} --color=always $1"
}

# So I don't accidentally delete anything again
alias rm="rm -i"
alias mv="mvg -gi" # mv with progress bar
alias cp="cpg -gi" # cp with progress bar

# Help Command Alias
autoload -U run-help
autoload run-help-git
autoload run-help-svn
autoload run-help-svk
alias help=run-help

# Aliases for ls
alias la="ls -a"       # Show all files including dotfiles in directory
alias l.="ls -d .*"    # Show only dotfiles current directory
alias ll="ls -l --git" # Show files in directory in long format
alias lla="ls -la"     # Show all files including dotfiles in directory in long format

# wezterm imgcat
alias imgcat="wezterm imgcat"

# Run yadm for system files
alias sysyadm="sudo yadm -Y /etc/yadm"

# Render and view context free art in one command
alias cfdg-view="cfdg --display=imv --"

# Sets up ssh-agent and adds ssh key at default location
function ssh-setup() {
	eval "$(ssh-agent -s)" && ssh-add
}

# Fallback behaviour if bd has no arguments passed
function bd-wrapper() {
	if [[ -z "$*" ]]; then
		cd ..
	else
		\bd "$@"
	fi
}

alias bd="bd-wrapper"

# Undo last cd
alias dc="cd -"

# zcompile all config files
function zcomp() {
	for i in ~/.zshrc ~/.p10k.zsh ~/.config/zsh/config.d/*.zsh; do
		zcompile $i && echo "zcompile: $i"
	done
}

# Autoload zsh modules not enabled by default
autoload zcalc           # Calculator program
autoload zmv             # Move/rename files that match a pattern/
autoload -U tetriscurses # Tetris
alias tetris="tetriscurses"

# Enable zoxide
eval "$(zoxide init zsh)"
alias cd="z"
