# ~/.config/zsh/config.d/a_first.zsh
# Script to be run first on zsh startup.

### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" ||           \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for                   \
    zdharma-continuum/z-a-rust         \
    zdharma-continuum/z-a-as-monitor   \
    zdharma-continuum/z-a-patch-dl     \
    zdharma-continuum/z-a-bin-gem-node 
	# zdharma-continuum/z-a-man

### End of Zinit's installer chunk

# Zinit Plugins

# Theme
zinit ice depth=1;
zinit light romkatv/powerlevel10k

# Plugins
# Replace ranger snippet with version from main repo
# when it gets merged.
zinit light-mode wait lucid for                                                                    \
		https://raw.githubusercontent.com/toonn/ranger/automatic-cd/examples/shell_automatic_cd.sh \
		https://raw.githubusercontent.com/wez/wezterm/main/assets/shell-integration/wezterm.sh     \
		OMZP::git                                                                                  \
		OMZP::command-not-found                                                                    \
		OMZP::fancy-ctrl-z                                                                         \
		zsh-users/zsh-history-substring-search                                                     \
		Aloxaf/fzf-tab                                                                             \
		amstrad/oh-my-matrix                                                                       \
		Tarrasch/zsh-bd                                                                            \
		hlissner/zsh-autopair                                                                      \
	atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay"                                        \
		zdharma-continuum/fast-syntax-highlighting                                                           \
	blockf                                                                                         \
		zsh-users/zsh-completions                                                                  \
	atload"!_zsh_autosuggest_start"                                                                \
       	zsh-users/zsh-autosuggestions

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
