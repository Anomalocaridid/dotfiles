# nnn config

export NNN_PLUG="s:!zsh -i;z:autojump;u:getplugs"
export NNN_BMS="d:$HOME/Documents;D:$HOME/Downloads;p:$HOME/Pictures;v:$HOME/Videos;e:$HOME/exercism"
export NNN_FIFO="/tmp/nnn.fifo"
export NNN_COLORS=#1909c9d02e0d2cff

# Sync subshell PWD with nnn
nnn_cd() {
	if [ -n "$NNN_PIPE" ]; then
		printf "%s\0" "0c${PWD}" ! > "${NNN_PIPE}" &
	fi
}

trap nnn_cd EXIT

# Some things rely on environment variables
# That may not be otherwise accessible when calling nnn through other means like Sway hotkeys
# Also I don't want to have to manually source multiple zsh scripts whenever I do something like that.
# Because I have to run a terminal emulator and have it execute zsh and make it run nnn and source nnn.zsh
source $HOME/.config/zsh/config.d/env.zsh
