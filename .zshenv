# ~/.zshenv
# Environment variables for zsh login shell

CARGO_BIN="$HOME/.cargo/bin" # Location of binaries installed by Rust's Cargo
STACK_BIN="$HOME/.local/bin" # Location of binaries installed by Haskell's Stack

path+=("$CARGO_BIN" "$STACK_BIN")

export EDITOR="xdg-open" # MIGHT rely on xdg-utils-handlr
export VISUAL="$EDITOR"

# Set LS_COLORS
eval "$(dircolors)"

# fzf configuration
export FZF_DEFAULT_OPTS="--color 'bg+:#091833,pointer:#ea00d9,prompt:#ea00d9'"

# Opt out of Microsoft's telemetry when using .NET SDK
export DOTNET_CLI_TELEMETRY_OPTOUT=true

# nnn config
NNN_PLUGIN_DIR="$HOME/.config/nnn/plugins"

export NNN_OPTS="cDEirx"
export NNN_OPENER="$NNN_PLUGIN_DIR/nuke"
export GUI=1
export NNN_BMS="c:$HOME/.config;d:$HOME/Documents;D:$HOME/Downloads;e:$HOME/exercism;p:$HOME/Pictures;v:$HOME/Videos;"
export NNN_PLUG="c:chksum;d:dragdrop;k:kdeconnect;M:nmount;n:bulknew;s:!zsh -i;u:!$NNN_PLUGIN_DIR/getplugs master;z:autojump;"
export NNN_FIFO="/tmp/nnn.fifo"
export NNN_COLORS="#1909c9d02e0d2cff"

# Sync subshell PWD with nnn
nnn_cd() {
	if [ -n "$NNN_PIPE" ]; then
		printf "%s\0" "0c${PWD}" ! > "${NNN_PIPE}" &
	fi
}

trap nnn_cd EXIT

#alias nnn="n"
