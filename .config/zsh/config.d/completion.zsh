# ~/.config/zsh/config.d/completion.zsh
# Completion settings for zsh.

# Autocompletion
autoload -Uz compinit && compinit              # Enables zsh tab-completion

# Zstyles 
zstyle ":completion:*" rehash true                        # Persistent rehash
zstyle ":completion:*" matcher-list "m:{a-zA-z}={A-Za-z}" # Case-insensitive completion
# fzf-tab styles
zstyle ":completion:complete:*:options" sort false        # Disable sort when completing options
zstyle ':completion:*:git-checkout:*' sort false          # Disable sort when completing git branches
zstyle ':completion:*:descriptions' format '[%d]'         # Add descriptions when able
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}     # color output with LS_COLORS
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always $realpath' # Show contents of directories
