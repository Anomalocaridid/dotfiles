{ config, pkgs, ... }: {
  # dependencies for widgets
  home.packages = with pkgs; [
    socat
    jc # Various scripts and commands
    jq # Various scripts and commands
    playerctl # Music info
  ];
  programs.eww = {
    enable = true;
    # Add tray with dynamic icon support
    # Remove once eww has a new release
    package = pkgs.eww.overrideAttrs (oldAttrs: rec {
      version = "unstable-2024-03-30";
      src = pkgs.fetchFromGitHub {
        owner = "elkowar";
        repo = "eww";
        rev = "1b819fb6469a3fb7fa9eb39d73eaefeb768be027";
        hash = "sha256-4jpIvjwFIvv2SpboOEXqL1GV5GnphxbZX20HND5Tjeo=";
      };
      cargoDeps = oldAttrs.cargoDeps.overrideAttrs (oldDeps: {
        inherit src;
        lockFile = "${src}/Cargo.lock";
        outputHash = "sha256-jEDl4AT8r7CycloB9pSD3tgzBIDDbB17/UPuXu60kr8=";
      });
      buildInputs = (oldAttrs.buildInputs or [ ]) ++ [
        pkgs.libdbusmenu
        pkgs.libdbusmenu-gtk3
      ];
    });
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
