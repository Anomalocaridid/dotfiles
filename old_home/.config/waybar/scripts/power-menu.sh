#!/bin/bash

entries=(Logout Suspend Hibernate Reboot Shutdown)

selected=$(printf '%s\n' "${entries[@]}" | wofi --conf="$HOME/.config/wofi/config.power" --style="$HOME/.config/wofi/style.widgets.css")

case $selected in
	Logout)
		swaymsg exit
		;;
	Suspend | Hibernate | Reboot)
		exec systemctl ${selected,,}
		;;
	Shutdown)
		systemctl poweroff -i
		;;
esac
