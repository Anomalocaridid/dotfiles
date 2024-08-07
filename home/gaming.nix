{
  config,
  pkgs,
  inputs,
  ...
}:
{
  home.packages = with pkgs; [
    gamescope # Used by Lutris for control over game resolution
    lutris
    packwiz # minecraft modpack creator
    prismlauncher
    pysolfc
    runelite
    sgt-puzzles
  ];

  # Enable wine-ge's fsync support
  home.sessionVariables.WINEFSYNC = 1;

  xdg.dataFile."PrismLauncher/themes/catppuccin".source =
    inputs.catppuccin-prismlauncher + /themes/${config.catppuccin.flavor}/${config.catppuccin.accent};
}
