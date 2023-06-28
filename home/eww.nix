{ pkgs, ... }: {
  # dependencies for widgets
  home.packages = with pkgs; [
    socat
    jq # Various scripts and commands
    playerctl # Music info
  ];
  programs.eww = {
    enable = true;
    package = pkgs.eww-wayland;
    configDir = ./.config/eww;
  };
  # Needed for music widget
  services.playerctld.enable = true;
}
