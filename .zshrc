export PATH=$HOME/bin:/usr/local/bin:$HOME/.local/bin:$PATH

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

# Show all dotfiles current working directory
alias l.="ls -d .*"

# ll Alias
alias ll="ls -l"

# Make ls output in color by default
alias ls="ls --color"

# Sets up ssh-agent and adds ssh key at default location
function ssh-setup() {eval "$(ssh-agent -s)" && ssh-add}

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh

# Signifies that this file loaded as planned.
echo "~/.zshrc loaded successfully" | lolcat -a
