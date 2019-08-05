export PATH=$HOME/bin:/usr/local/bin:$HOME/.local/bin:$PATH

# Persistent rehash
zstyle ':completion:*' rehash true

# Check if zplug is installed
if [[ ! -d ~/.zplug ]]; then
	git clone https://github.com/zplug/zplug ~/.zplug
	source ~/.zplug/init.zsh && zplug update --self
fi

source ~/.zplug/init.zsh

# Syntax highlighting
zplug "zsh-users/zsh-syntax-highlighting"

# Extends auto completion
zplug "zsh-users/zsh-completions"

# Oh-My-Zsh's git plugin
zplug "plugins/git", from:oh-my-zsh

# Theme
zplug "oskarkrawczyk/honukai-iterm-zsh", as:theme

# Install packages that have not ben installed yet
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

# Sets up ssh-agent and adds ssh key at default location
function ssh-setup() {eval "$(ssh-agent -s)" && ssh-add}

# Signifies that this file loaded as planned.
echo "~/.zshrc loaded successfully" | lolcat -a
