{ writeShellApplication, nixos-install-tools, ... }:
writeShellApplication {
  name = "generate-hardware-config.sh";
  runtimeInputs = [ nixos-install-tools ];
  text = ''
    # Sanity options for safety
    set -o errtrace \
    	-o errexit \
    	-o nounset \
    	-o xtrace \
    	-o pipefail

    # Config constants
    readonly MOUNT_DIR="''${2:-}"                                                    # Where drive is mounted by disko (set by disko, not config)
    readonly PERSIST_DIR="/persist"                                                    # Persistent partition mount location
    readonly CONFIG_DIR="$MOUNT_DIR$PERSIST_DIR/etc/nixos"                             # Config location in persistant partition
    readonly HOST_CONFIG_DIR="$CONFIG_DIR/nixos-configurations/''${1:-$ (hostname)}" # Subdirectory containing host-specific config

    # Show user where config will be created just in case
    echo "Generating hardware config in $HOST_CONFIG_DIR"
    # Will either leave config untouched or update hardware-configuration.nix and nothing else
    nixos-generate-config --no-filesystems --root "$MOUNT_DIR" --dir "$HOST_CONFIG_DIR"
  '';
}
