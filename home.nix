{ config, pkgs, ... }:

{
  home.username = "anomalocaris";
  home.homeDirectory = "/home/anomalocaris";
  home.packages = with pkgs; [
    git
    helix
    wezterm
  ];

  home.persistence."/persist/home/anomalocaris" = {   
    allowOther = true;
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
      ".ssh"         # SSH key
    ];
  };

  # DON'T TOUCH
  home.stateVersion = "22.11";

  programs.home-manager.enable = true;

  # Temporary
  # Just the bare minimum to commit setup so far
  home.file.".gitconfig".source = ./home/.gitconfig;
}
