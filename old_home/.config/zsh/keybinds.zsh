# ~/.config/zsh/config.d/keybinds.zsh
# Keybindings for zsh.

# Keybindings for history-substring-search
# bind UP and DOWN arrow keys
zmodload zsh/terminfo
# bindkey "${terminfo[kcuu1]}" history-substring-search-up
# bindkey "${terminfo[kcud1]}" history-substring-search-down

# # bind UP and DOWN arrow keys (compatibility fallback
# # for Ubuntu 12.04, Fedora 21, and MacOSX 10.9 users)
# bindkey "^[[A" history-substring-search-up
# bindkey "^[[B" history-substring-search-down

# # bind k and j for VI mode
# bindkey -M vicmd "k" history-substring-search-up
# bindkey -M vicmd "j" history-substring-search-down

# Make the delete key work like a delete key
bindkey "^[[3~" delete-char

# Vi-style line editing
#bindkey -v
