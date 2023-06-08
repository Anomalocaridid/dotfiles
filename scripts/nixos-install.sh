#!/usr/bin/env bash
# nixos-install.sh: install NixOS with my config

# Sanity options for safety
set -o errtrace \
	-o errexit \
	-o nounset \
	-o xtrace \
	-o pipefail

# Config constants
# NOTE: Remove branch argument before merge
readonly CONFIG_REPO="Anomalocaridid/dotfiles"
readonly FLAKE="github:$CONFIG_REPO/nixos#home-pc"

readonly MOUNT_DIR="/mnt"
readonly PERSIST_DIR="/persist"
readonly CONFIG_DIR="$MOUNT_DIR$PERSIST_DIR/etc/nixos"

# Partition disk with disko
echo "Partitioning disk with disko"
nix run github:nix-community/disko \
	--extra-experimental-features nix-command \
	--extra-experimental-features flakes \
	-- \
	--flake "$FLAKE" \
	--mode zap_create_mount </dev/tty

echo "Cloning config repo"
# NOTE: Remove branch argument before merge
git clone -b nixos "https://github.com/$CONFIG_REPO.git" "$CONFIG_DIR"
git -C "$CONFIG_DIR" remote set-url origin "git@github.com:$CONFIG_REPO.git"

# Will either leave config untouched or update hardware-configuration.nix and nothing else
echo "Generating hardware config"
nixos-generate-config --no-filesystems --root "$MOUNT_DIR" --dir "$PERSIST_DIR"

# Generate password hashes
echo "Setting password (xtrace disabled)"
# Call chpasswd.sh from it's location in config directory
"$CONFIG_DIR"/scripts/chpasswd.sh "$MOUNT_DIR"

# Install NixOS
echo "Installing NixOS and rebooting"
nixos-install --flake "git+file://$CONFIG_DIR#home-pc" --no-root-passwd
#reboot
