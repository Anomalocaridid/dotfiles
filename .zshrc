#!/bin/false
# vim:fileencoding=utf-8:foldmethod=marker

# Environment Variables {{{

# $PATH declaration {{{

# Location of binaries installed by Rust's Cargo
CARGO_BIN=$HOME/.cargo/bin

# Location of binaries installed by Haskell's Stack
STACK_BIN=$HOME/.local/bin

# Folder for scripts
SCRIPTS=$HOME/Scripts

HOME_BIN=$HOME/bin

export PATH=$CARGO_BIN:$STACK_BIN:$SCRIPTS:$HOME_BIN:/usr/local/bin:$PATH
fpath+=$HOME/.zfunc

# }}}

export EDITOR="kak"

# Program-specific Variables {{{

# bat {{{
export BAT_THEME="Dracula"
# }}}

# }}}

# }}}

# Autostart tmux {{{
# As long as tmux is not already started.
# And you are in an interactive shell.
if [[ -t 0 ]] && [[ -z "$TMUX" ]] && [[ $- = *i* ]]; then
    #tmux attach &> /dev/null ||
    exec tmux
fi
# }}}

# Ascii Terminal greeting. {{{
# Shows Linux distro and version in rainbow ascii art.
echo -en "\e[1m"
lsb_release --description --release --short | tr -d '"' | toilet -t -f smslant -F border | lolcat
echo -e "\e[1m Welcome back, $USER!\e[0m\n" | lolcat

# }}}

# Enable Powerlevel10k instant prompt. {{{
# Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# }}}

# Autocompletion {{{
# Enables zsh tab-completion
autoload -Uz compinit && compinit

# Completion for kitty
kitty + complete setup zsh | source /dev/stdin

# Zstyles {{{
# Persistent rehash
zstyle ':completion:*' rehash true

# Case-insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-zA-z}={A-Za-z}'

# }}}

# }}}

# Zplug Plugins {{{

# Check if zplug is installed {{{
if [[ ! -d ~/.zplug ]]; then
    git clone https://github.com/zplug/zplug ~/.zplug
    source ~/.zplug/init.zsh && zplug update --self
fi

# }}}

# All plugins should be below this command.
source ~/.zplug/init.zsh

# zplug self management
zplug 'zplug/zplug', hook-build:'zplug --self-manage'

# Powerlevel10k Theme
zplug "romkatv/powerlevel10k", use:powerlevel10k.zsh-theme

# zsh-users {{{

# Syntax highlighting
zplug "zsh-users/zsh-syntax-highlighting", defer:2

# Extends auto completion
zplug "zsh-users/zsh-completions"

# Search history based on already entered text
zplug "zsh-users/zsh-history-substring-search"

# Auto-suggestions
zplug "zsh-users/zsh-autosuggestions"

# }}}

# Oh-My-Zsh {{{

# Oh-My-Zsh's git plugin
zplug "plugins/git", from:oh-my-zsh

# Change directory based on history
zplug "plugins/z", from:oh-my-zsh

# Adds short command to reload and recompile zsh config
zplug "plugins/zsh_reload", from:oh-my-zsh

# ESC twice to prefix current or previous command with sudo
zplug "plugins/sudo", from:oh-my-zsh

# Warp to directory
zplug "plugins/wd", from:oh-my-zsh

# Fish-like interactive cd
# Requires fzf
zplug "plugins/zsh-interactive-cd", from:oh-my-zsh

# Aliases for Arch-based Linux distros
zplug "plugins/archlinux", from:oh-my-zsh

# Alias finder
zplug "plugins/alias-finder", from:oh-my-zsh

# Provides suggestions for packages to be installed if a common command is not found
# Requires pkgfile
zplug "plugins/command-not-found", from:oh-my-zsh

# }}}

# Install packages that have not been installed yet {{{
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -rq; then
        echo; zplug install
    else
        echo
    fi
fi

# }}}

# All plugins should be above this command
# Load installed plugins
zplug load

# Clean unused plugins
zplug clean

# }}}

# Keybindings {{{

# Keybindings for history-substring-search
# bind UP and DOWN arrow keys
zmodload zsh/terminfo
bindkey "${terminfo[kcuu1]}" history-substring-search-up
bindkey "${terminfo[kcud1]}" history-substring-search-down

# bind UP and DOWN arrow keys (compatibility fallback
# for Ubuntu 12.04, Fedora 21, and MacOSX 10.9 users)
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# bind k and j for VI mode
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# Make the delete key work like a delete key
bindkey "^[[3~" delete-char

# }}}

# Aliases {{{

# Aliases alternative programs to commonly used commands
alias ls="exa"

alias cat="bat"
alias less="bat --paging=always"
alias grep="batgrep"
alias man="batman"
alias diff="batdiff"
batthemes() {
	bat --list-themes | fzf --preview="bat --theme={} --color=always $1"
}

# One character ~/.zshrc sourcing
alias .="src"

# So I don't accidentally delete anything again
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -i"

# Command to show the main drive's space at a glance
alias space="df -h --output='source,size,used,avail,pcent' /dev/sda1"

# Force tmux to use 256 colors
alias tmux="tmux -2"

# Help Command Alias {{{
autoload -U run-help
autoload run-help-git
autoload run-help-svn
autoload run-help-svk
alias help=run-help

# }}}

# Aliases for ls {{{
# Show all files including dotfiles in directory
alias la="ls -a"

# Show only dotfiles current directory
alias l.="ls -d .*"

# Show files in directory in long format
alias ll="ls -l"

# Show all files including dotfiles in directory in long format
alias lla="ls -la"

# }}}

# Aliases for kittens included with kitty terminal {{{
alias icat="kitty +kitten icat"          # Displays images in terminal.
alias d="kitty +kitten diff"             # Displays diffs between two files.
alias hints="kitty +kitten hints"        # Selects and acts on arbitrary text snippets on screen.
alias panel="kitty +kitten panel"        # Draws a gpu accelerated panel using another program's output.
alias clipboard="kitty +kitten clipboard" # Copy/paste to system clipboard.

# }}}

# Aliases for quick access to frequently edited dotfiles {{{
alias zshrc="$EDITOR ~/.zshrc"
alias kakrc="kak ~/.config/kak/kakrc ~/.config/kak-lsp/kak-lsp.toml"

# }}}

# Misc Aliases {{{

# A fabulous quote of the day, delivered by a cow.
alias moo="fortune | cowsay | lolcat"

# }}}

# Command Functions {{{
# Sets up ssh-agent and adds ssh key at default location
function ssh-setup() {
	eval "$(ssh-agent -s)" && ssh-add
}

# Sets zsh as shell.
function set-zsh() {
	chsh -s "$(which zsh)" && echo "All done! Please restart terminal."
}

# }}}

# }}}

# Autoload zsh modules not enabled by default {{{
# Calculator program
autoload zcalc

# Move/rename files that match a pattern/
autoload zmv

# Tetris
autoload -U tetriscurses
alias tetris="tetriscurses"

# }}}

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh
