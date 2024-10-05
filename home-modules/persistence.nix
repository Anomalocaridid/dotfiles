{ config, ... }:
{
  home.persistence."/persist/${config.home.homeDirectory}" = {
    allowOther = true;
    directories = [
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
      ".mozilla" # Firefox data
      "Projects" # Misc. programming
      "qmk_firmware" # QMK firmware
      "qmk_userspace" # QMK userspace
      "quickemu" # quickemu VMs
      ".ssh" # SSH key
      "Sync" # Syncthing
      ".tuxpaint" # Tux Paint saves
      # Non-critical caches and data to persist
      ".cache/keepassxc" # KeePassXC cache
      ".cache/spotify" # Spotify cache
      ".cache/tealdeer" # Tldr pages, prevents tealdeer redownloading them every time
      ".config/armcord" # ArmCord user data
      ".config/exercism" # Exercism API key
      ".config/GIMP" # GIMP settings
      ".config/keepassxc" # KeePassXC settings
      ".config/LanguageTool" # LanguageTool settings
      ".config/libreoffice" # Libreoffice settings
      ".config/spotify" # Spotify user data
      ".config/strawberry" # Strawberry settings
      ".config/syncthing" # Syncthing settings
      ".local/share/com.yubico.authenticator" # Yubico auth settings (may have secrets?)
      ".local/share/strawberry" # Strawberry cache
      ".local/share/Tabletop Simulator" # Tabletop Simulator settings
      ".local/share/zathura" # Zathura bookmarks, etc.
      ".local/share/zoxide" # Zoxide history
      ".mix" # Contains hex needed for elixir-ls
      ".unison" # Unison codebase, needs to be persistent as all added code ends up there
    ];
  };
}
