{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
{
  home.packages =
    with pkgs;
    let
      wine-ge = inputs.nix-gaming.packages.${pkgs.system}.wine-ge;
    in
    [
      gamescope # Used by Lutris for control over game resolution
      lutris
      packwiz # minecraft modpack creator
      parsec-bin # Online multiplayer for local multiplayer games
      prismlauncher
      pysolfc
      runelite
      sgt-puzzles
      wine-ge # System-level install for Lutris
    ];

  # Enable wine-ge's fsync support
  home.sessionVariables.WINEFSYNC = 1;

  ssbm.slippi-launcher = {
    # enable = true;
    isoPath = "${config.home.homeDirectory}/Documents/Super Smash Bros. Melee (USA) (En,Ja) (Rev 2).ciso";
  };

  xdg =
    let
      palette =
        (lib.importJSON "${config.catppuccin.sources.palette}/palette.json")
        .${config.catppuccin.flavor}.colors;
    in
    {
      dataFile."PrismLauncher/themes/catppuccin-${config.catppuccin.flavor}".source =
        let
          capitalFlavor =
            (
              string:
              (lib.strings.toUpper (builtins.substring 0 1 string))
              + (builtins.substring 1 (builtins.stringLength string) string)
            )
              config.catppuccin.flavor;
        in
        pkgs.runCommand "catppuccin-prismlauncher-theme" { } ''
          mkdir -p $out
          cp -r ${inputs.catppuccin-prismlauncher}/themes/${capitalFlavor}/* $out
          substituteInPlace $out/theme.json \
            --replace '"Highlight": "#b4befe"' '"Highlight": "${palette.${config.catppuccin.accent}.hex}"'
        '';
    };
}
