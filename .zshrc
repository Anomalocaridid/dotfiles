# ~/.zshrc

# Source separate config files
for conf in "$HOME/.config/zsh/config.d/"*.zsh; do
	source "${conf}"
done
unset conf
