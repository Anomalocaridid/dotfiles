{ pkgs, ... }:

{
  home.username = "anomalocaris";
  home.homeDirectory = "/home/anomalocaris";
  home.packages = with pkgs; [
    neofetch
    (nerdfonts.override { fonts = [ "FiraCode" ]; })
    firefox # fallback browser
    nixpkgs-fmt
    nil
  ];


  imports = map (n: ./. + "/home/${n}") (builtins.attrNames (builtins.readDir ./home));

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
      "Sync" # Syncthing
      "exercism" # Exercism
      "Projects" # Misc. programming
      "qmk_firmware" # QMK
      ".ssh" # SSH key
      # Caches I need to persist
      ".cache/bat" # bat cache --build
    ];
  };

  # DON'T TOUCH
  home.stateVersion = "22.11";

  programs.home-manager.enable = true;

}
