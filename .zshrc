#!/bin/false

export PATH=$HOME/scripts:$HOME/bin:/usr/local/bin:$HOME/.local/bin:$PATH
fpath+=$HOME/.zfunc

# Custom Keybindings

# Pressing up and down moves forward or back, respectively, in the terminal history based on what is already typed.
bindkey '\e[A' history-beginning-search-backward
bindkey '\e[B' history-beginning-search-forward

# Persistent rehash
zstyle ':completion:*' rehash true

# Check if zplug is installed
if [[ ! -d ~/.zplug ]]; then
    git clone https://github.com/zplug/zplug ~/.zplug
    source ~/.zplug/init.zsh && zplug update --self
fi

source ~/.zplug/init.zsh

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

# Install packages that have not been installed yet
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    else
        echo
    fi
fi

zplug load

# Enables zsh tab-completion
autoload -U compinit && compinit

# Help Command Alias
autoload -U run-help
autoload run-help-git
autoload run-help-svn
autoload run-help-svk
alias help=run-help

# Show all files including dotfiles in directory
alias la="ls -A"

# Show only dotfiles current directory
alias l.="ls -d .*"

# Show files in directory in long format
alias ll="ls -l"

# Show all files including dotfiles in directory in long format
alias lla="ls -lA"

# Make ls output in color by default
alias ls="ls --color"

# One character ~/.zshrc sourcing
alias .=". ~/.zshrc"

# Sets up ssh-agent and adds ssh key at default location
function ssh-setup() {eval "$(ssh-agent -s)" && ssh-add}

# Sets zsh as shell.
function set-zsh() {chsh -s $(which zsh) && echo "All done! Please restart terminal."}

# Autoload bult in commands not enabled by default
autoload zcalc
autoload zmv

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh

# Signifies that this file loaded as planned.
echo "~/.zshrc loaded successfully" | lolcat
