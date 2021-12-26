# ~/.config/zsh/config.d/a_first.zsh
# Script to be run first on zsh startup.

# Clone zcomet if necessary
if [[ ! -f ${ZDOTDIR:-${HOME}}/.zcomet/bin/zcomet.zsh ]]; then
  command git clone https://github.com/agkozak/zcomet.git ${ZDOTDIR:-${HOME}}/.zcomet/bin
fi

# Source zcomet
source ${ZDOTDIR:-${HOME}}/.zcomet/bin/zcomet.zsh

# Install prompt theme
zcomet load romkatv/powerlevel10k

# Plugins
zcomet load	Aloxaf/fzf-tab # Use fzf for tab completion
zcomet load	amstrad/oh-my-matrix # Falling text
zcomet load	hlissner/zsh-autopair # Automatically close and delete paired delimiters
zcomet load	Tarrasch/zsh-bd # Move to higher level directory
zcomet load zsh-users/zsh-history-substring-search # Fish-style history search

# Oh My Zsh plugins
zcomet load ohmyzsh plugins/command-not-found # Shows which package an uninstalled command is in
zcomet load ohmyzsh plugins/fancy-ctrl-z # Hit Ctrl z instead of running fg

# Snippets
zcomet snippet https://github.com/jarun/nnn/blob/master/misc/quitcd/quitcd.bash_zsh
zcomet snippet https://github.com/wez/wezterm/blob/main/assets/shell-integration/wezterm.sh

# Load these plugins last and in this order
zcomet load zdharma-continuum/fast-syntax-highlighting # Fast syntax highlighting (not really maintained)
zcomet load zsh-users/zsh-autosuggestions # Fish-style autosuggestions

# Run compinit and compile its cache
zcomet compinit

# Ascii Terminal greeting. 
# Shows Linux distro and version in rainbow ascii art.
echo -en "\e[1m"
lsb_release --description --release --short | tr -d '"' | toilet -t -f smslant -F border | lolcat -t
echo -e "\e[1m Welcome back, $USER!\e[0m\n" | lolcat -t

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
 if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
 	source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
 fi
