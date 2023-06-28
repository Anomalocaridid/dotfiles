#!/usr/bin/env bash
# toggle-var.sh: toggle given eww var

# Sanity options for safety
set -o errtrace \
	-o errexit \
	-o nounset \
	-o pipefail

var=$1

case $(eww get "$var") in
"true") new_val="false" ;;
"false") new_val="true" ;;
*)
	echo "ERROR: '$var' is not a boolean"
	exit 1
	;;
esac

eww update "$var=$new_val"
