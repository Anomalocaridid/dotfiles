# ~/.config/zsh/config.d/env.zsh
# Environment variables for zsh.

CARGO_BIN="$HOME/.cargo/bin" # Location of binaries installed by Rust's Cargo
STACK_BIN="$HOME/.local/bin" # Location of binaries installed by Haskell's Stack

path+=("$CARGO_BIN" "$STACK_BIN")

export EDITOR="hx"
export VISUAL="$EDITOR"

# Set LS_COLORS
eval "$(dircolors)"

# fzf configuration
export FZF_DEFAULT_OPTS="--color 'bg+:#091833,pointer:#ea00d9,prompt:#ea00d9'"

# Opt out of Microsoft's telemetry when using .NET SDK
export DOTNET_CLI_TELEMETRY_OPTOUT=true
