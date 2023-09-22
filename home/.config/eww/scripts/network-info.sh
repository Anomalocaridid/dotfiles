#!/usr/bin/env bash
# network-info.sh: get current connection info

# Sanity options for safety
set -o errtrace \
	-o errexit \
	-o nounset \
	-o pipefail

nmcli device wifi list |
	"$HOME"/.config/eww/scripts/network-info.awk -v \
		vpn_status="$(nmcli device status | awk '$2 == "wireguard" { print $3 }')"
