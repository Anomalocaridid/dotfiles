{ config, pkgs, ... }:
{
  # dependencies for widgets
  home.packages = with pkgs; [
    socat
    jc # Various scripts and commands
    jq # Various scripts and commands
    playerctl # Music info
  ];
  programs.eww = {
    enable = true;
    configDir = ./.config/eww;
  };
  # Needed for music widget
  services.playerctld.enable = true;

  xdg.configFile =
    let
      fonts = config.stylix.fonts;
      # TODO: Figure out how to use config.catppuccin.sources.palette instead
      paletteFile = pkgs.fetchurl {
        url = "https://unpkg.com/@catppuccin/palette@1.1.1/scss/_${config.catppuccin.flavor}.scss";
        hash = "sha256-5tnz9xd+8LNXQ5Dmxhcm/SkqM51LFKcsZLu/NIN0cxU=";
      };
    in
    {
      # Needed to ensure other files can be inserted into eww directory
      "eww".recursive = true;
      "eww/_palette.scss".text =
        (builtins.readFile paletteFile)
        + "$accent: \$${config.catppuccin.accent};\n"
        + "$font: '${fonts.monospace.name}'";
    };
}
