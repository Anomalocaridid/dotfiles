#!/usr/bin/env bash
# network-info.sh: get current connection info

# Sanity options for safety
# No pipefail or else it won't work when disconnected
set -o errtrace \
	-o errexit \
	-o nounset

icon() {
	bars="$1"

	if [[ $(cat /sys/class/net/w*/operstate) = "up" ]]; then
		case $bars in
		"____" | "") echo "󰤯" ;;
		"▂___" | "*") echo "󰤟" ;;
		"▂▄__" | "**") echo "󰤢" ;;
		"▂▄▆_" | "***") echo "󰤥" ;;
		"▂▄▆█" | "****") echo "󰤨" ;;
		esac
	else
		echo "󰤮"
	fi
}

readonly FORMAT='{"name": "%s", "rate": "%d %s", "icon": "%s"}\n'

IFS=" "

connection="$(nmcli --fields IN-USE,SSID,RATE,BARS device wifi list |
	grep -- "^\*" |
	tr -s "$IFS" |
	cut --delimiter="$IFS" --fields=2- |
	xargs printf "$FORMAT")"

new_icon="$(icon "$(echo "$connection" | jq --raw-output '.icon')")"

echo "$connection" | jq --compact-output ".icon = \"$new_icon\""
