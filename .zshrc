#!/usr/bin/env false

# Environment Variables
CARGO_BIN=$HOME/.cargo/bin # Location of binaries installed by Rust's Cargo
STACK_BIN=$HOME/.local/bin # Location of binaries installed by Haskell's Stack
SCRIPTS=$HOME/Scripts      # Folder for scripts
HOME_BIN=$HOME/bin         # Folder for other programs

# $PATH declaration
export PATH=$CARGO_BIN:$STACK_BIN:$SCRIPTS:$HOME_BIN:/usr/local/bin:$PATH
fpath+=$HOME/.zfunc

# Use Kakoune as the default text editor.
export VISUAL="kak"
export EDITOR="kak"

# Ascii Terminal greeting. 
# Shows Linux distro and version in rainbow ascii art.
echo -en "\e[1m"
lsb_release --description --release --short | tr -d '"' | toilet -t -f smslant -F border | lolcat
echo -e "\e[1m Welcome back, $USER!\e[0m\n" | lolcat

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
	source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Autocompletion
autoload -Uz compinit && compinit              # Enables zsh tab-completion
kitty + complete setup zsh | source /dev/stdin # Completion for kitty

# Zstyles 
zstyle ':completion:*' rehash true                        # Persistent rehash
zstyle ':completion:*' matcher-list 'm:{a-zA-z}={A-Za-z}' # Case-insensitive completion

### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zinit-zsh/z-a-rust \
    zinit-zsh/z-a-as-monitor \
    zinit-zsh/z-a-patch-dl \
    zinit-zsh/z-a-bin-gem-node

### End of Zinit's installer chunk

# Zinit Plugins

# Theme
# Should be the first plugin
zinit ice depth=1;
zinit light romkatv/powerlevel10k

zinit wait lucid for \
		OMZP::git \
		OMZP::zsh_reload \
		OMZP::sudo \
		OMZP::wd \
		OMZP::zsh-interactive-cd \
		OMZP::archlinux \
		OMZP::alias-finder \
		OMZP::command-not-found \
	svn \
		OMZP::z \
		zsh-users/zsh-history-substring-search \
	atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
		zdharma/fast-syntax-highlighting \
	blockf \
		zsh-users/zsh-completions \
	atload"!_zsh_autosuggest_start" \
       	zsh-users/zsh-autosuggestions

# Keybindings

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

# Aliases 
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

# Help Command Alias 
autoload -U run-help
autoload run-help-git
autoload run-help-svn
autoload run-help-svk
alias help=run-help

# Aliases for ls 
alias la="ls -a"    # Show all files including dotfiles in directory
alias l.="ls -d .*" # Show only dotfiles current directory
alias ll="ls -l"    # Show files in directory in long format
alias lla="ls -la"  # Show all files including dotfiles in directory in long format

# Aliases for kittens included with kitty terminal 
alias icat="kitty +kitten icat"           # Displays images in terminal.
alias d="kitty +kitten diff"              # Displays diffs between two files.
alias hints="kitty +kitten hints"         # Selects and acts on arbitrary text snippets on screen.
alias panel="kitty +kitten panel"         # Draws a gpu accelerated panel using another program's output.
alias clipboard="kitty +kitten clipboard" # Copy/paste to system clipboard.

# Aliases for quick access to frequently edited dotfiles 
alias zshrc="$EDITOR ~/.zshrc"
alias kakrc="kak -e edit-kakrc"

# A fabulous quote of the day, delivered by a cow.
alias moo="fortune | cowsay | lolcat"

# Sets up ssh-agent and adds ssh key at default location
function ssh-setup() {
	eval "$(ssh-agent -s)" && ssh-add
}

# Sets zsh as the default shell for the current user.
function set-zsh() {
	chsh -s "$(which zsh)" && echo "All done! Please restart terminal."
}

# Autoload zsh modules not enabled by default 
autoload zcalc           # Calculator program
autoload zmv             # Move/rename files that match a pattern/
autoload -U tetriscurses # Tetris
alias tetris="tetriscurses"

# load Powerlevel10k configuration
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh

