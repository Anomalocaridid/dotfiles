#!/usr/bin/env bash
# pop_window.sh: toggle a given window in eww

# Sanity options for safety
set -o errtrace \
	-o errexit \
	-o nounset \
	-o pipefail

window=$1
var="$window-window"

~/.config/eww/scripts/toggle-var.sh "$var"

if [[ $(eww get "$var") == "true" ]]; then
	eww open "$window"
else
	eww close "$window"
fi
