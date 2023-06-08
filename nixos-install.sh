#!/usr/bin/env bash
# nixos-install.sh: install NixOS with my config

# Sanity options for safety
set -o errtrace \
	-o errexit \
	-o nounset \
	-o xtrace \
	-o pipefail

# Config constants
readonly USERNAME="anomalocaris"
# NOTE: Remove branch argument before merge
readonly CONFIG_REPO="Anomalocaridid/dotfiles"
readonly FLAKE="github:$CONFIG_REPO/nixos#home-pc"
readonly DISK="/dev/vda"
readonly MEMORY="8G"

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
	--mode zap_create_mount \
	--argstr disk "$DISK" \
	--argstr memory "$MEMORY" </dev/tty

echo "Cloning config repo"
# NOTE: Remove branch argument before merge
git clone -b nixos "https://github.com/$CONFIG_REPO.git" "$CONFIG_DIR"
git -C "$CONFIG_DIR" remote set-url origin "git@github.com:$CONFIG_REPO.git"

# Will either leave config untouched or update hardware-configuration.nix and nothing else
echo "Generating hardware config"
nixos-generate-config --no-filesystems --root "$MOUNT_DIR" --dir "$PERSIST_DIR"

# Generate password hashes
echo "Setting password (trace disabled)"
# Hide commands so password isn't printed to terminal
set +x

# Prompt for password
while true; do
	read -r -s -p "Password: " password </dev/tty
	echo ""
	read -r -s -p "Password (again): " password2 </dev/tty
	echo ""

	if [[ $password == "$password2" ]]; then
		if [[ -z $password ]]; then
			echo "ERROR: Password is empty. Please enter a different password."
		else
			break
		fi
	else
		echo "ERROR: Passwords do not match. Please re-enter password."
	fi
done

readonly PASSWORD_HASH_DIR="$MOUNT_DIR$PERSIST_DIR/passwords"
mkdir -p $PASSWORD_HASH_DIR
# Password may be the same, but calling mkpasswd twice ensures each hash has a different salt
mkpasswd "$password" --stdin --method=yescrypt >"$PASSWORD_HASH_DIR/root"
mkpasswd "$password" --stdin --method=yescrypt >"$PASSWORD_HASH_DIR/$USERNAME"
# Re-enable trace
set -x

# Install NixOS
echo "Installing NixOS and rebooting"
nixos-install --flake "git+file://$CONFIG_DIR#home-pc" --no-root-passwd
#reboot
