# ~/.config/kak/config.d/keymaps.kak
# Kakoune custom keymaps

# Math prompt powered by Qalculate!
# Requires Qalculate!
map global normal = ': prompt math: %{exec "a%val{text}<lt>esc> | qalc -t +u8 -f -<lt>ret>"}<ret>'

# Toggle visible whitespace
map global user w ": whitespace-toggle<ret>" -docstring "toggle whitespace"

# Align columns
map global user a "<a-s><S>\h<ret><a-;><&>" -docstring "align columns in selection"
map global user <a-a> "<percent><a-s><S>\h<ret><a-;><&>" -docstring "align columns in buffer"

# Open Tutorial
map global user t ": trampoline<ret>" -docstring "open a tutorial"

# Games user mode
declare-user-mode games
map global user g ": enter-user-mode games<ret>" -docstring "games and fun"
map global games r ": roguelight-toggle<ret>" -docstring "toggle roguelight"
map global games m ": roguelight-map<ret>" -docstring "roguelight map"
map global games s ": prompt -init 1 choose-sokoban-level: %{%{sokoban %val{text}}<ret>}" -docstring "sokoban"

# Extra mappings
map global normal D "<a-l>d" -docstring "delete to end of line"
map global normal Y "<a-l>y" -docstring "yank to end of line"

map global normal "#" ": comment-line<ret>" -docstring "comment line"
map global normal "<a-#>" ": comment-block<ret>" -docstring "comment block"

map global normal <ret> ": execute-keys xdp<ret>" -docstring "move current line one line down"
map global normal <a-ret> ": execute-keys xdkkp<ret>" -docstring "move current line one line up"

map global goto m "<esc>m;" -docstring "matching char"
