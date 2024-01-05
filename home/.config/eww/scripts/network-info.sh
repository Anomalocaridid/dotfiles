#!/usr/bin/env bash
# network-info.sh: get current connection info

# Sanity options for safety
set -o errtrace \
	-o errexit \
	-o nounset \
	-o pipefail

# Use secure icons if VPN is connected
vpn_status=$(jc nmcli device status |
	jq --raw-output '.[] | select(.type=="wireguard") | .state')

if [[ $vpn_status == "connected" ]]; then
	icons="󰤬 󰤡 󰤤 󰤧 󰤪"
else
	icons="󰤯 󰤟 󰤢 󰤥 󰤨"
fi

nmcli device wifi list |
	awk -v icon_string="$icons" '
      function icon(bars) {
        split(icon_string, icons)
        len = length(bars)
        # Check if stars (****) or bars (▂▄▆█)
        if (test ~ /*/ || len == 0) {
          # Stars
          if (len == 0) {
            len += 1
          }
          return icons[len]
        } else {
          # Bars
          return icons[match(bars, /[^_]/) + 1]
        }
      }

      BEGIN {
        FORMAT = "{\"name\": \"%s\", \"rate\": \"%s\", \"icon\": \"%s\", \"connected\": \"%s\"}\n"
        matched = 0
      }

      $1 == "*" { 
        matched = 1
        printf FORMAT, $3, $6 " " $7, icon($9), "true"
      }

      END {
        if (!matched) {
          printf FORMAT, "<Disconnected>", "N/A", "󰤮", "false"
        }
      }
	'
