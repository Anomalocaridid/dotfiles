#!/bin/env sh
# Times the screen off and puts it to background
swayidle \
    timeout  300 'swaymsg "output * dpms off"' \
    resume 'swaymsg "output * dpms on"' &
# Saves current focused workspace
workspace=$(swaymsg -t get_workspaces | jq "map(select(.focused)) | .[].num")
# Goes to empty workspace
swaymsg workspace number 0
# Locks the screen immediately
swaylock
# Restores previous workspace
swaymsg workspace number "$workspace"
# Kills last background task so idle timer doesn't keep running
kill %%
