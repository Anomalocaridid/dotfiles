{ pkgs, ... }: {
  home.packages = with pkgs; [
    gamescope # Used by Lutris for control over game resolution
    lutris
    packwiz # minecraft modpack creator
    prismlauncher-qt5 # Non-qt5 version does not work as well with theme
    # TODO: Upstream this change to pysolfc in nixpkgs
    (pysolfc.overrideAttrs (oldAttrs: {
      music = fetchzip {
        url = "https://versaweb.dl.sourceforge.net/project/pysolfc/PySol-Music/PySol-Music-4.50/pysol-music-4.50.tar.xz";
        sha256 = "sha256-sOl5U98aIorrQHJRy34s0HHaSW8hMUE7q84FMQAj5Yg=";
      };

      postInstall = (oldAttrs.postInstall or "") + ''
        mkdir $out/share/PySolFC/music
        cp -r $music/data/music/* $out/share/PySolFC/music
      '';
    }))
    sgtpuzzles
  ];
}
