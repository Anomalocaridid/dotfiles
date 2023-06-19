{ osConfig, ... }: {
  home.packages = [ osConfig.nur.repos.milahu.spotify-adblock ];
  xdg.desktopEntries.spotify = {
    name = "Spotify";
    genericName = "Music Player";
    icon = "spotify-client";
    exec = "spotify-adblock --url=%U";
    terminal = false;
    mimeType = [ "x-scheme-handler/spotify" ];
    categories = [ "Audio" "Music" "Player" "AudioVideo" ];
  };
}
