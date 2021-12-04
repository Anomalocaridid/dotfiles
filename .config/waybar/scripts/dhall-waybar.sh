#!/usr/bin/env bash

WAYBAR_DIR="$HOME/.config/waybar"

dhall-to-json --file "$WAYBAR_DIR/config.dhall" > "$WAYBAR_DIR/config"
