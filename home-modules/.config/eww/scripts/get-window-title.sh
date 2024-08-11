#!/usr/bin/env sh
# get-window-title.sh: get the title of the currently focused window
# Based on a script from https://wiki.hyprland.org/hyprland-wiki/pages/Useful-Utilities/Status-Bars/#eww

hyprctl activewindow -j | jq --raw-output .title

# They are awk arguments, not bash arguments
# shellcheck disable=SC2016
socat -u UNIX-CONNECT:/"$XDG_RUNTIME_DIR"/hypr/"$HYPRLAND_INSTANCE_SIGNATURE"/.socket2.sock - | stdbuf -o0 awk -F '>>|,' '/^activewindow>>/{print $3}'
