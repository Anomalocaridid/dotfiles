# ~/.zshrc
# Clone zcomet if necessary
if [[ ! -f ${ZDOTDIR:-${HOME}}/.zcomet/bin/zcomet.zsh ]]; then
  command git clone https://github.com/agkozak/zcomet.git ${ZDOTDIR:-${HOME}}/.zcomet/bin
fi

# Source zcomet
source ${ZDOTDIR:-${HOME}}/.zcomet/bin/zcomet.zsh

# Ascii Terminal greeting. 
# Shows Linux distro and version in rainbow ascii art.
echo -en "\e[1m"
lsb_release --description --release --short | tr -d '"' | toilet -t -f smslant -F border | lolcat -t
echo -e "\e[1m Welcome back, $USER!\e[0m" | lolcat -t

# Source separate config files
for conf in "$HOME/.config/zsh/"*.zsh; do
	zcomet snippet "${conf}"
done
unset conf
