{
  config,
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

  xdg.dataFile."PrismLauncher/themes/catppuccin".source =
    inputs.catppuccin-prismlauncher + /themes/${config.catppuccin.flavor}/${config.catppuccin.accent};
}
