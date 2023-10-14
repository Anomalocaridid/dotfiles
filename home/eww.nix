{ config, pkgs, ... }: {
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

  xdg.configFile =
    let
      fonts = config.stylix.fonts;
    in
    {
      # Needed to ensure other files can be inserted into eww directory
      "eww".recursive = true;
      "eww/_palette.scss".text =
        (builtins.readFile
          (pkgs.custom.catppuccin-palette-files
          + "/share/scss/_${config.catppuccin.flavour}.scss"))
        + "$accent: \$${config.catppuccin.accent};\n"
        + "$font: '${fonts.monospace.name}'";
    };
}
