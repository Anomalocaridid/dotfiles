{ config, self, ... }:
{
  perSystem =
    {
      lib,
      pkgs,
      self',
      ...
    }:
    {
      packages.install = pkgs.writeShellApplication {
        name = "install.sh";
        runtimeInputs = with pkgs; [
          git
          nix
          nixos-install-tools
          disko
          # Custom script
          self'.packages.chpasswd
        ];
        text = ''
          # Sanity options for safety
          set -o errtrace \
          	-o errexit \
          	-o nounset \
          	-o xtrace \
          	-o pipefail

          # Config constants
          readonly CONFIG_REPO="${config.flake.meta.gitHubUsername}/dotfiles" # Dotfile config repo name
          readonly FLAKE="github:$CONFIG_REPO"                                # Flake URL
          readonly MOUNT_DIR="/mnt"                                           # Where drive is mounted by disko (set by disko, not config)
          readonly PERSIST_DIR="${config.flake.meta.persistDir}"              # Persistent partition mount location
          readonly CONFIG_DIR="$MOUNT_DIR$PERSIST_DIR/etc/nixos"              # Config location in persistant partition

          # List of available NixOS configurations
          ${lib.toShellVar "device_list" (lib.attrNames self.nixosConfigurations)}

          # Select config from flake to install
          PS3="Select device config to install: "

          select device in "''${device_list[@]}" "quit"; do
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
            		break
            		;;
          	esac
          done

          readonly device

          # Select mode to run disko in
          PS3="Select disko mode: "

          select mode in "destroy,format,mount" "format,mount" "mount" "quit"; do
            case $mode in
            	"quit")
            		echo "Aborting install"
            		exit
            		;;
            	"")
            		echo "ERROR: Invalid selection '$REPLY'"
            		REPLY=""
            		;;
            	*)
            		break
            		;;
            esac
          done

          readonly mode

          # Prompt for confirmation
          PS3="Proceed with running disko in '$mode' mode and installing '$device' config? "

          select response in "Proceed" "Cancel"; do
            case $response in
              "Proceed")
                echo "Proceeding with installation"
                break
                ;;
              "Cancel")
                echo "Cancelling installation"
                exit
                ;;
              "")
                echo "ERROR: Invalid selection '$REPLY'"
                REPLY=""
                ;;
            esac
          done

          echo "Partitioning disk with disko"
          disko --flake "$FLAKE/rework-install-script#$device" --mode "$mode"

          echo "Cloning config repo"
          git clone "https://github.com/$CONFIG_REPO.git" "$CONFIG_DIR"
          git -C "$CONFIG_DIR" remote set-url origin "git@github.com:$CONFIG_REPO.git"

          echo "Setting password (xtrace disabled)"
          chpasswd.sh "$MOUNT_DIR"

          echo "Installing NixOS"
          nixos-install --flake "git+file://$CONFIG_DIR#$device" --no-channel-copy --no-root-passwd
        '';
      };
    };
}
