{ config, self, ... }:
{
  perSystem =
    {
      lib,
      pkgs,
      inputs',
      ...
    }:
    {
      packages.install = pkgs.writeShellApplication {
        name = "install.sh";
        runtimeInputs = with pkgs; [
          disko
          git
          nix
          nixos-facter
          nixos-install-tools
          inputs'.hpf-passwd.packages.hpf-passwd
        ];
        text = ''
          # Safety options
          set -o xtrace            # Prints executed commands to the terminal
          set -o errexit           # Stop executing script when a command fails
          set -o errtrace          # Re-enables ERR trap disabled by errexit
          set -o nounset           # Stop executing script when an unset variable is accessed
          set -o pipefail          # Propagate non-zero exit codes to the end of a pipeline
          shopt -s inherit_errexit # Allow subshells to inherit errexit setting, requires bash >=4.4

          # Config constants
          readonly USERNAME="${config.flake.meta.username}"
          readonly CONFIG_REPO="https://github.com/${config.flake.meta.gitHubUsername}/dotfiles.git" # Dotfile config repo name
          readonly MOUNT_DIR="/mnt"                                                                  # Where drive is mounted by disko (set by disko, not config)
          readonly PERSIST_DIR="${config.flake.meta.persistDir}"                                     # Persistent partition mount location
          readonly CONFIG_DIR="$MOUNT_DIR$PERSIST_DIR/etc/nixos"                                     # Config location in persistant partition

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

          select mode in "mount" "format,mount" "destroy,format,mount" "quit"; do
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

          temp_dir="$(mktemp --directory)"
          readonly temp_dir
          # Ensure $temp_dir is cleaned up after script exists
          trap 'rm -rf -- "$temp_dir"' EXIT

          # Clone config repo up front to use as a single source of truth and reduce the risk of TOCTOU bugs
          echo "Cloning config repo into temporary directory"
          git clone "$CONFIG_REPO" "$temp_dir"
          git -C "$temp_dir" remote set-url origin "$CONFIG_REPO"

          echo "Updating NixOS Facter report"
          nixos-facter --output "$temp_dir/hosts/$device/facter.json"

          echo "Partitioning disk with disko"
          disko --flake "git+file://$temp_dir#$device" --mode "$mode"

          echo "Copying config repo onto system"
          if [[ -d "$CONFIG_DIR" ]]; then
            rm --recursive --force "$CONFIG_DIR"
          fi
          mkdir --parents "$(dirname "$CONFIG_DIR")"
          cp --recursive --no-preserve=all "$temp_dir" "$CONFIG_DIR"

          echo "Setting password"
          hpf-passwd --extra-experimental-features "flakes" --flake "git+file://$CONFIG_DIR#$device" --prefix "$MOUNT_DIR" "$USERNAME"

          echo "Installing NixOS"
          nixos-install --flake "git+file://$CONFIG_DIR#$device" --no-channel-copy --no-root-passwd
        '';
      };
    };
}
