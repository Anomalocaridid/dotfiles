#!/usr/bin/env bash
# change-active-workspace.sh: increment or decrement active workspace
# Based on a script from https://wiki.hyprland.org/hyprland-wiki/pages/Useful-Utilities/Status-Bars/#eww

# Sanity options for safety
set -o errtrace \
	-o errexit \
	-o nounset \
	-o pipefail

direction=$1
current=$2

target=$current

case "$direction" in
"up")
	((target++))
	;;
"down")
	((target--))
	;;
*) exit ;;
esac

readonly MAX=10

# Non-positive values are more or less functionally the same as 1, so no need to check for them
if ((target > MAX)); then
	target=$MAX
fi

echo "jumping to $target"
hyprctl dispatch workspace "$target"
