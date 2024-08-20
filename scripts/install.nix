{
  writeShellApplication,
  git,
  jq,
  nix,
  nixos-install-tools,
  disko,
  generate-hardware-config,
  chpasswd,
  ...
}:
writeShellApplication {
  name = "install.sh";
  runtimeInputs = [
    git
    jq
    nix
    nixos-install-tools
    disko
    # Custom scripts
    generate-hardware-config
    chpasswd
  ];
  text = ''
    # Sanity options for safety
    set -o errtrace \
    	-o errexit \
    	-o nounset \
    	-o xtrace \
    	-o pipefail

    # Config constants
    readonly CONFIG_REPO="Anomalocaridid/dotfiles"         # Dotfile config repo name
    readonly FLAKE="github:$CONFIG_REPO"                   # Flake URL
    readonly MOUNT_DIR="/mnt"                              # Where drive is mounted by disko (set by disko, not config)
    readonly PERSIST_DIR="/persist"                        # Persistent partition mount location
    readonly CONFIG_DIR="$MOUNT_DIR$PERSIST_DIR/etc/nixos" # Config location in persistant partition

    # Select config from flake to install
    PS3="Select device config to install: "
    # Dynamically retrieve list of available configs
    device_list="$(nix --extra-experimental-features "nix-command flakes" flake show --json $FLAKE |
      jq --raw-output ".nixosConfigurations | keys[]")"

    select device in $device_list "quit"; do
    	case $device in
    	"quit")
    		echo "Aborting install"
    		exit
    		;;
    	"")
    		echo "ERROR: Invalid selection '$REPLY'"
    		REPLY=""
    		;;
    	*)
    		echo "Installing $device config"
    		break
    		;;
    	esac
    done

    echo "Partitioning disk with disko"
    disko --flake "$FLAKE#$device" --mode disko

    echo "Cloning config repo"
    git clone "https://github.com/$CONFIG_REPO.git" "$CONFIG_DIR"
    git -C "$CONFIG_DIR" remote set-url origin "git@github.com:$CONFIG_REPO.git"

    echo "Generating hardware config"
    generate-hardware-config.sh "$device" "$MOUNT_DIR"

    echo "Setting password (xtrace disabled)"
    chpasswd.sh "$MOUNT_DIR"

    echo "Installing NixOS"
    nixos-install --flake "git+file://$CONFIG_DIR#$device" --no-channel-copy --no-root-passwd
  '';
}
