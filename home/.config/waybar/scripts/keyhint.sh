#!/usr/bin/env bash

keybindings=(
	"ESC" "close this app" ""
	"=" "modkey" "(set mod Mod4)"
	"+enter" "Terminal" "(wezterm)"
	"+d" "Application Menu" "(wofi)"
	"+p" "Activities" "(wofi)"
	"+o" "" "Open Browser"
	"+n" "" "Open Files"
	"+q" "close focused app" "(kill)"
	"[Shift]+Print-key" "screenshot" "(grim)"
	"+c" "clear clipboard" "(wl-copy)"
	"+Shift+e" "power-menu" "(wofi)"
	"+t" "open keybinding helper" "full list"
)

yad \
	--title="EndeavourOS Sway-WM keybindings:" \
	--no-buttons \
	--width=425 \
	--height=350 \
	--center \
	--list \
	--column=Key \
	--column=Description \
	--column=Command \
	"${keybindings[@]}"
