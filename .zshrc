#!/bin/false

export PATH=$HOME/.cargo/bin:$HOME/scripts:$HOME/bin:/usr/local/bin:$HOME/.local/bin:$PATH
fpath+=$HOME/.zfunc

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
# Check if zplug is installed
if [[ ! -d ~/.zplug ]]; then
    git clone https://github.com/zplug/zplug ~/.zplug
    source ~/.zplug/init.zsh && zplug update --self
fi

source ~/.zplug/init.zsh

# zplug self management
zplug 'zplug/zplug', hook-build:'zplug --self-manage'

# Syntax highlighting
zplug "zsh-users/zsh-syntax-highlighting", defer:2

# Extends auto completion
zplug "zsh-users/zsh-completions"

# Oh-My-Zsh's git plugin
zplug "plugins/git", from:oh-my-zsh

# Powerlevel10k Theme
zplug "romkatv/powerlevel10k", use:powerlevel10k.zsh-theme

# Colored man pages
zplug "plugins/colored-man-pages", from:oh-my-zsh

# Change directory based on history
zplug "plugins/z", from:oh-my-zsh

# Search history based on already entered text
zplug "zsh-users/zsh-history-substring-search"

# Syntax highlighted file viewer
# Requires Pygments
zplug "plugins/colorize", from:oh-my-zsh

# Auto-suggestions
zplug "zsh-users/zsh-autosuggestions"

# Vi-like functionality
zplug "plugins/vi-mode", from:oh-my-zsh

# Adds short command to reload and recompile zsh config
zplug "plugins/zsh_reload", from:oh-my-zsh

# ESC twice to prefix current or previous command with sudo
zplug "plugins/sudo", from:oh-my-zsh

# Warp to directory
zplug "plugins/wd", from:oh-my-zsh

# Fish-like interactive cd
# requires fzf
zplug "plugins/zsh-interactive-cd", from:oh-my-zsh

# Install packages that have not been installed yet
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -rq; then
        echo; zplug install
    else
        echo
    fi
fi

zplug load
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
# Help Command Alias {{{
autoload -U run-help
autoload run-help-git
autoload run-help-svn
autoload run-help-svk
alias help=run-help

# }}}

# Aliases alternative programs to commonly used commands
alias ls="exa"
alias vim="nvim"

# Ohmyzsh's colorize plugin commands
alias cat="ccat"
alias less="cless"

# One character ~/.zshrc sourcing
#alias .=". ~/.zshrc"
alias .="src"

# So I don't accidentally delete anything again
alias rm="rm -i"
alias mv="mv -i"

# Command to show the main drive's space at a glance
alias space="df -h --output='source,size,used,avail,pcent' /dev/sda1"

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

# Misc Aliases {{{
# A helpful cow reminds you that you are not in vim.
function not-in-vim() {
	cowsay "You aren't in vim, dummy!" | lolcat
}
alias :w="not-in-vim"
alias :x="not-in-vim"

# Vim style exiting.
alias :q="exit"

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

# Autoload built in commands not enabled by default
autoload zcalc
autoload zmv

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh
