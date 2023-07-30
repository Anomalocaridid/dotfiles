{ pkgs, ... }: {
  home.packages = with pkgs; [
    gamescope # Used by Lutris for control over game resolution
    lutris
    packwiz # minecraft modpack creator
    parsec-bin # Online multiplayer for local multiplayer games
    prismlauncher-qt5 # Non-qt5 version does not work as well with theme
    pysolfc
    sgtpuzzles
  ];
}
