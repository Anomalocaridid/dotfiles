#!/usr/bin/env -S awk -f
# network-info.awk: format info for network-info.sh

BEGIN {
  FORMAT = "{\"name\": \"%s\", \"rate\": \"%s\", \"icon\": \"%s\", \"connected\": \"%s\"}\n"
  if (vpn_status == "connected") {
    split("󰤬 󰤡 󰤤 󰤧 󰤪", icons)
  } else {
    split("󰤯 󰤟 󰤢 󰤥 󰤨", icons)
  }
  matched = 0
}

$1 == "*" { 
  matched = 1
  printf FORMAT, $3, $6 " " $7, icon(icons, $9), "true"
}

END {
  if (!matched) {
    printf FORMAT, "<Disconnected>", "N/A", "󰤮", "false"
  }
}

function icon(icons, bars) {
    switch (bars) {
      case "____":
      case "":
        return icons[1]

      case "▂___":
      case "**":
        return icons[2]

      case "▂▄__":
      case "***":
        return icons[3]

      case "▂▄▆_":
      case "****":
        return icons[4]

      case "▂▄▆█":
      case "*****":
        return icons[5]
    }
}
