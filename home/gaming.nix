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
      prismlauncher
      pysolfc
      runelite
      sgt-puzzles
      wine-ge # System-level install for Lutris
    ];

  # Enable wine-ge's fsync support
  home.sessionVariables.WINEFSYNC = 1;

  xdg.dataFile."PrismLauncher/themes/catppuccin".source =
    inputs.catppuccin-prismlauncher + /themes/${config.catppuccin.flavor}/${config.catppuccin.accent};
}
