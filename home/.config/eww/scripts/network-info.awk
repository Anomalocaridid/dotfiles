#!/usr/bin/env -S awk -f
# network-info.awk: format info for network-info.sh

BEGIN {
  FORMAT = "{\"name\": \"%s\", \"rate\": \"%s\", \"icon\": \"%s\"}\n"
  if (vpn_status == "connected") {
    split("󰤬 󰤡 󰤤 󰤧 󰤪", icons)
  } else {
    split("󰤯 󰤟 󰤢 󰤥 󰤨", icons)
  }
  matched = "false"
}

$1 == "*" { 
  matched = "true"
  printf FORMAT, $3, $6 " " $7, icon(icons, $9)
}

END {
  if (matched == "false") {
    printf FORMAT, "<Disconnected>", "N/A", "󰤮"
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
