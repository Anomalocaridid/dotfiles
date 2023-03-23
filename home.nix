{ config, pkgs, ... }:

{
  home.username = "anomalocaris";
  home.homeDirectory = "/home/anomalocaris";

  home.packages = with pkgs; [
    git
    helix
    wezterm
  ];

  # DON'T TOUCH
  home.stateVersion = "22.11";

  programs.home-manager.enable = true;

  # Temporary
  # Just the bare minimum to commit setup so far
  home.file.".gitconfig".source = ./home/.gitconfig;
}
