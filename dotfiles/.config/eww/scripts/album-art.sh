#!/usr/bin/env bash
# album-art.sh: download album art using url from playerctl

# Sanity options for safety
set -o errtrace \
	-o errexit \
	-o nounset \
	-o pipefail

readonly ALBUM_ART="/tmp/playerctl_album_art.tmp"

playerctl --follow metadata mpris:artUrl | while read -r url; do
	curl --fail --silent --show-error --location --retry 3 "$url" -o "$ALBUM_ART"
	# Signals to eww to update album art
	echo "$ALBUM_ART"
done
