# ~/.config/zsh/plugins.zsh
# Plugins to load
# Source after zcomet clone, before terminal greeting

# Install prompt theme
#zcomet load romkatv/powerlevel10k

# Plugins
zcomet load Aloxaf/fzf-tab # Use fzf for tab completion
zcomet load hlissner/zsh-autopair # Automatically close and delete paired delimiters
zcomet load zsh-users/zsh-history-substring-search # Fish-style history search
zcomet trigger bd Tarrasch/zsh-bd # Move to higher level directory
zcomet trigger matrix amstrad/oh-my-matrix # Falling text

# Oh My Zsh plugins
zcomet load ohmyzsh lib/termsupport.zsh # Set terminal window title
zcomet load ohmyzsh plugins/fancy-ctrl-z # Hit Ctrl z instead of running fg
zcomet trigger command_not_found_handler ohmyzsh plugins/command-not-found # Shows which package an uninstalled command is in

# Snippets
zcomet snippet https://github.com/jarun/nnn/blob/master/misc/quitcd/quitcd.bash_zsh
zcomet snippet https://github.com/wez/wezterm/blob/main/assets/shell-integration/wezterm.sh

# Load these plugins last and in this order
zcomet load zdharma-continuum/fast-syntax-highlighting # Fast syntax highlighting (not really maintained)
zcomet load zsh-users/zsh-autosuggestions # Fish-style autosuggestions

# Run compinit and compile its cache
zcomet compinit
