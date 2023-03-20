#!/usr/bin/env bash
# nixos-install.sh: install NixOS with my config
# Sanity options for safety
set -Eeuxo pipefail

# Config constants
readonly USERNAME="anomalocaris"
readonly CONFIG_REPO="Anomalocaridid/dotfiles"
readonly DISK="/dev/vda"
readonly MEMORY="8G"

# Clone dotfiles
# NOTE: Remove branch argument before merge
echo "Cloning config repo to temporary location"
git clone -b nixos "https://github.com/$CONFIG_REPO.git" /tmp/dotfiles

# Partition disk with disko
echo "Partitioning disk with disko"
nix run github:nix-community/disko \
	--extra-experimental-features nix-command \
	--extra-experimental-features flakes \
	-- \
	--mode zap_create_mount "/tmp/dotfiles/disko-config.nix" \
	--argstr disk "$DISK" \
	--argstr memory "$MEMORY" </dev/tty

# Moving dotfiles to final location
# NOTE: Change to location in /persist after initial config
echo "Moving config repo to mounted disk"
mkdir -p /mnt/etc
mv /tmp/dotfiles /mnt/etc/nixos

# Generate hardware-config.nix
# Disko will handle the filesystems and an existing configuration.nix won't be overridden
# NOTE: Write to dotfiles' location in /persist after initial config
echo "Generating hardware config"
nixos-generate-config --no-filesystems --root /mnt

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

readonly PASSWORD_HASH_DIR="/mnt/persist/passwords"
mkdir -p $PASSWORD_HASH_DIR
# Password may be the same, but calling mkpasswd twice ensures each hash has a different salt
mkpasswd "$password" --stdin --method=yescrypt >"$PASSWORD_HASH_DIR/root"
mkpasswd "$password" --stdin --method=yescrypt >"$PASSWORD_HASH_DIR/$USERNAME"
# Re-enable trace
set -x

# Install NixOS
# NOTE: Uncomment after initial config
# echo "Installing NixOS and rebooting"
# nixos-install --no-root-passwd
# reboot
