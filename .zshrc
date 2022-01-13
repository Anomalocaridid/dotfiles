# ~/.zshrc
# Clone zcomet if necessary
if [[ ! -f ${ZDOTDIR:-${HOME}}/.zcomet/bin/zcomet.zsh ]]; then
  command git clone https://github.com/agkozak/zcomet.git ${ZDOTDIR:-${HOME}}/.zcomet/bin
fi

# Source zcomet
source ${ZDOTDIR:-${HOME}}/.zcomet/bin/zcomet.zsh

zcomet snippet ~/.config/zsh/plugins.zsh

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

# Source separate config files
for conf in "$HOME/.config/zsh/config.d/"*.zsh; do
	zcomet snippet "${conf}"
done
unset conf

# load Powerlevel10k configuration
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ -f ~/.p10k.zsh ]] && zcomet snippet ~/.p10k.zsh
