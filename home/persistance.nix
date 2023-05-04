{ config, ... }: {
  home.persistence."/persist/${config.home.homeDirectory}" = {
    allowOther = true;
    directories = [
      # Default directories I care about
      "Documents"
      "Downloads"
      "Music"
      "Pictures"
      "Videos"
      # Other important stuff
      "Sync" # Syncthing
      "exercism" # Exercism
      "Projects" # Misc. programming
      "qmk_firmware" # QMK
      ".ssh" # SSH key
      # Caches and data to persist
      ".local/share/zoxide" # zoxide history
    ];
  };

}
