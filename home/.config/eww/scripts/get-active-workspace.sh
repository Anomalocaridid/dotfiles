#!/usr/bin/env bash
# get-active-workspace.sh: get the index of the currently focused workspace
# Based on a script from https://wiki.hyprland.org/hyprland-wiki/pages/Useful-Utilities/Status-Bars/#eww

# Sanity options for safety
set -o errtrace \
	-o errexit \
	-o nounset \
	-o pipefail

hyprctl monitors -j | jq '.[] | select(.focused) | .activeWorkspace.id'

# They are awk arguments, not bash arguments
# shellcheck disable=SC2016
socat -u UNIX-CONNECT:/tmp/hypr/"$HYPRLAND_INSTANCE_SIGNATURE"/.socket2.sock - |
	stdbuf -o0 awk -F '>>|,' -e '/^workspace>>/ {print $2}' -e '/^focusedmon>>/ {print $3}'
