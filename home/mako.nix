{ config, pkgs, ... }: {
  # For custom notifications for scripting with notify-send
  home.packages = with pkgs; [ libnotify ];
  services.mako = {
    enable = true;
    font = "Fira Code Nerd Font 12.5";
    backgroundColor = "#000b1e";
    textColor = "#0abdc6";
    width = 315;
    height = 200;
    padding = "10";
    margin = "10";
    progressColor = "over #711c91";
    iconPath = "${config.gtk.iconTheme.package}";
    maxIconSize = 70;
    layer = "overlay";
    borderSize = 5;
    borderRadius = 5;
    borderColor = "#ea00d9";
    defaultTimeout = 5000;
    extraConfig = #ini
      ''
        [urgency=high]
        ignore-timeout=1
      '';
  };
}
