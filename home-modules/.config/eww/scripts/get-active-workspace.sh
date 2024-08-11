#!/usr/bin/env bash
# get-active-workspace.sh: get the index of the currently focused workspace
# Based on a script from https://wiki.hyprland.org/hyprland-wiki/pages/Useful-Utilities/Status-Bars/#eww

# Sanity options for safety
set -o errtrace \
	-o errexit \
	-o nounset \
	-o pipefail

active-space() {
	hyprctl monitors -j | jq '.[] | select(.focused) | .activeWorkspace.id'
}

active-space
# Get id of active space, when workspace or focused monitor changes
socat -u UNIX-CONNECT:/"$XDG_RUNTIME_DIR"/hypr/"$HYPRLAND_INSTANCE_SIGNATURE"/.socket2.sock - |
	stdbuf -o0 awk -F '>>|,' -e '/^(workspace|focusedmon)>>/' |
	while read -r _; do
		active-space
	done
