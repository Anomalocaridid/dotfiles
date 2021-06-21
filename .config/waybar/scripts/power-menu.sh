#!/bin/bash

entries=(Logout Suspend Hibernate Reboot Shutdown)

selected=$(printf '%s\n' "${entries[@]}" | wofi --conf="$HOME/.config/wofi/config.power" --style="$HOME/.config/wofi/style.widgets.css" | awk '{print tolower($1)}')

case $selected in
	logout)
		swaymsg exit
		;;
	suspend)
		exec systemctl suspend
		;;
	hibernate)
		exec systemctl hibernate
		;;
	reboot)
		exec systemctl reboot
		;;
	shutdown)
		exec systemctl poweroff -i
		;;
esac
