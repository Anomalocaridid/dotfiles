# ~/.config/zsh/config.d/env.zsh
# Environment variables for zsh.

CARGO_BIN=$HOME/.cargo/bin # Location of binaries installed by Rust's Cargo
STACK_BIN=$HOME/.local/bin # Location of binaries installed by Haskell's Stack
SCRIPTS=$HOME/Scripts      # Folder for scripts
HOME_BIN=$HOME/bin         # Folder for other programs

# $PATH declaration
export PATH=$CARGO_BIN:$STACK_BIN:$SCRIPTS:$HOME_BIN:/usr/local/bin:$PATH
fpath+=$HOME/.zfunc

# Use Kakoune as the default text editor.
export VISUAL="kak"
export EDITOR="kak"

# Set LS_COLORS
eval "$(dircolors)"

# fzf configuration
export FZF_DEFAULT_OPTS="--color 'bg+:#091833,pointer:#ea00d9,prompt:#ea00d9'"

# Opt out of Microsoft's telemetry when using .NET SDK
export DOTNET_CLI_TELEMETRY_OPTOUT=true
