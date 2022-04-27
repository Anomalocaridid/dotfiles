#!/bin/env bash
# Times the screen off and puts it to background
swayidle \
    timeout  300 'swaymsg "output * dpms off"' \
    resume 'swaymsg "output * dpms on"' &
# Saves current focused workspace
workspace=$(swaymsg -t get_workspaces | jq "map(select(.focused)) | .[].num")
# Goes to empty workspace
swaymsg workspace number 0
# Hides bar
swaymsg bar mode invisible
# Locks the screen immediately
# Detaches swaylock to run next commands while locked
swaylock --daemonize
# Shows bar
swaymsg bar mode dock
# Restores previous workspace
swaymsg workspace number "$workspace"
# Waits for swaylock to terminate so idle timer is not prematurely killed
while pgrep swaylock &> /dev/null; do
    :
done
# Kills last background task so idle timer doesn't keep running
kill %%
