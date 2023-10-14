{ config, lib, pkgs, ... }: {
  home.packages = with pkgs; [
    gamescope # Used by Lutris for control over game resolution
    lutris
    packwiz # minecraft modpack creator
    parsec-bin # Online multiplayer for local multiplayer games
    prismlauncher
    # prismlauncher-qt5 # Non-qt5 version does not work as well with theme
    pysolfc
    sgt-puzzles
  ];

  ssbm.slippi-launcher = {
    enable = true;
    isoPath = "${config.home.homeDirectory}/Documents/Super Smash Bros. Melee (USA) (En,Ja) (Rev 2).ciso";
  };

  xdg.dataFile."PrismLauncher/themes/catppuccin-${config.catppuccin.flavour}".source =
    let
      capitalFlavour = (string:
        (lib.strings.toUpper (builtins.substring 0 1 string))
        + (builtins.substring 1 (builtins.stringLength string) string)
      ) config.catppuccin.flavour;
      themeDirectory = "share/catppuccin-${config.catppuccin.flavour}";
      palette = pkgs.custom.catppuccin-palette.${config.catppuccin.flavour};
    in
    (pkgs.stdenvNoCC.mkDerivation rec {
      name = "catppuccin-prismlauncher-theme";
      version = "2023-10-17_1697558708";
      src = pkgs.fetchzip {
        url = "https://github.com/PrismLauncher/Themes/releases/download/${version}/Catppuccin-${capitalFlavour}.zip";
        hash = "sha256-QJ1pLSQFDn7p0d68MafSBp4cp5E2Bk5yqBqnOqdbu10=";
      };

      installPhase = ''
        runHook preInstall

        mkdir -p $out/${themeDirectory}
        cp -r $src/* $out/${themeDirectory}

        runHook postInstall
      '';

      # Inspired by https://github.com/catppuccin/prismlauncher/issues/1
      postInstall = ''
        substituteInPlace $out/${themeDirectory}/theme.json \
          --replace '"Highlight": "#b4befe"' '"Highlight": "#${palette.${config.catppuccin.accent}.hex}"'
      '';
    }) + "/${themeDirectory}";
}
