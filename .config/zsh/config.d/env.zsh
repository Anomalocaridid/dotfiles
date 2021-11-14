# ~/.config/zsh/config.d/env.zsh
# Environment variables for zsh.

export EDITOR="kak"
export VISUAL="$EDITOR"

# Set LS_COLORS
eval "$(dircolors)"

# fzf configuration
export FZF_DEFAULT_OPTS="--color 'bg+:#091833,pointer:#ea00d9,prompt:#ea00d9'"

# Opt out of Microsoft's telemetry when using .NET SDK
export DOTNET_CLI_TELEMETRY_OPTOUT=true
