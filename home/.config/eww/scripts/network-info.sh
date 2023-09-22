#!/usr/bin/env bash
# network-info.sh: get current connection info

# Sanity options for safety
# No pipefail or else it won't work when disconnected
set -o errtrace \
	-o errexit \
	-o nounset

WIFI_ICONS=("󰤯" "󰤟" "󰤢" "󰤥" "󰤨")
VPN_ICONS=("󰤬" "󰤡" "󰤤" "󰤧" "󰤪")

icon() {
	bars="$1"
	vpn_status="$2"

	# All wireless interfaces in Linux start with wl
	# Note that wireguard interfaces start with wg and should be excluded
	if [[ $(cat /sys/class/net/wl*/operstate) = "up" ]]; then
		if [[ $vpn_status = "connected" ]]; then
			icons=("${VPN_ICONS[@]}")
		else
			icons=("${WIFI_ICONS[@]}")
		fi

		case $bars in
		"____" | "") echo "${icons[1]}" ;;
		"▂___" | "*") echo "${icons[2]}" ;;
		"▂▄__" | "**") echo "${icons[3]}" ;;
		"▂▄▆_" | "***") echo "${icons[4]}" ;;
		"▂▄▆█" | "****") echo "${icons[5]}" ;;
		esac
	else
		echo "󰤮"
	fi
}

connection="$(
	nmcli device wifi list |
		# Get SSID, rate (in two fields), and bars
		awk '$1 == "*" { printf "{\"name\": \"%s\", \"rate\": \"%d %s\", \"icon\": \"%s\"}\n", $3, $6, $7, $9 }'
)"

wireguard_status="$(nmcli device status | awk '$2 == "wireguard" { print $3 }')"

new_icon="$(icon "$(echo "$connection" | jq --raw-output '.icon')" "$wireguard_status")"

echo "$connection" | jq --compact-output ".icon = \"$new_icon\""
