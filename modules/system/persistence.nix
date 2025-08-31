{ config, inputs, ... }:
let
  inherit (config.flake.meta) username persistDir;
in
{
  flake.meta = rec {
    persistDir = "/persist";
    passwordDir = "${persistDir}/passwords";
  };

  unify.modules.persistence.nixos =
    { config, ... }:
    {
      imports = [ inputs.impermanence.nixosModules.impermanence ];

      environment.persistence.${persistDir} = {
        hideMounts = true;
        directories = [
          # Necessary system state
          ## NixOS
          "/var/lib/nixos" # Holds state needed for stable uids and gids for users and groups
          ## systemd
          "/var/lib/systemd" # Systemd state directory, used for numerous things
          # Other important things
          "/etc/NetworkManager/system-connections" # Network connections
          "/var/lib/clamav" # ClamAV signature database
          # Nix config
          {
            directory = "/etc/nixos";
            user = username;
            group = config.users.users.${username}.group;
          }
        ];
        files = [
          # Necessary system state
          ## systemd
          "/etc/machine-id" # Unique system id for logging, etc.
        ];
        users.${username}.directories = [
          # Default directories I care about
          "Documents"
          "Downloads"
          "Games"
          "Music"
          "Pictures"
          "Videos"

          # Gaming-specific stuff
          ".cache/lutris" # Lutris banner cache
          ".config/lutris" # Lutris games and settings
          ".config/unity3d" # Needed for some games' settings
          ".local/share/lutris" # Lutris runtime data
          ".local/share/PrismLauncher" # Prism Launcher data
          ".local/share/Steam" # Steam games and save data
          ".PySolFC" # PySolFC settings and save data
          ".runelite" # Runelite settings and cache

          # Other important stuff
          ".config/FreeTube" # Freetube user data
          "exercism" # Exercism
          ".librewolf" # Librewolf data
          "Projects" # Misc. programming
          "qmk_firmware" # QMK firmware
          "qmk_userspace" # QMK userspace
          "quickemu" # quickemu VMs
          "Sync" # Syncthing
          ".tuxpaint" # Tux Paint saves
          # SSH key
          {
            directory = ".ssh";
            mode = "u=rwx,g=,o=";
          }

          # Non-critical caches and data to persist
          ".cache/spotify" # Spotify cache
          ".cache/tealdeer" # Tldr pages, prevents tealdeer redownloading them every time
          ".config/exercism" # Exercism API key
          ".config/GIMP" # GIMP settings
          ".config/LanguageTool" # LanguageTool settings
          ".config/libreoffice" # Libreoffice settings
          ".config/OrcaSlicer" # Bambu Studio login and printer settings
          ".config/spotify" # Spotify user data
          ".config/strawberry" # Strawberry settings
          ".config/vesktop/sessionData" # Vesktop user data
          ".local/share/com.yubico.authenticator" # Yubico auth settings (may have secrets?)
          ".local/share/strawberry" # Strawberry cache
          ".local/share/Tabletop Simulator" # Tabletop Simulator settings
          ".local/share/zathura" # Zathura bookmarks, etc.
          ".local/share/zoxide" # Zoxide history
          ".local/state/syncthing" # Syncthing settings
        ];
      };
      # Needed for root to access bind mounted dirs created by impermanence
      programs.fuse.userAllowOther = true;
    };
}
