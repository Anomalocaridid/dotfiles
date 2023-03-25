{
  "/persist" = {
    hideMounts = true;
    directories = [
      "/etc/nixos"
    ];
    users.anomalocaris = {
      directories = [
        # Default directories I care about
        "Documents"
        "Downloads"
        "Music"
        "Pictures"
        "Videos"
        # Other important stuff
        "Sync"         # Syncthing
        "exercism"     # Exercism
        "Projects"     # Misc. programming
        "qmk_firmware" # QMK
        { directory = ".ssh"; mode="700"; } # SSH key
      ];
    };
  };
}
