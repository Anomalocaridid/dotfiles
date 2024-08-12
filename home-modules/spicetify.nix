{
  config,
  pkgs,
  inputs,
  ...
}:
let
  spicePkgs = inputs.spicetify-nix.legacyPackages.${pkgs.system};
in
{
  programs.spicetify = {
    enable = true;
    theme = spicePkgs.themes.catppuccin // {
      requiredExtensions =
        let
          name = "accent.js";
        in
        [
          {
            src =
              pkgs.writeTextDir name # javascript
                ''
                  localStorage.setItem("catppuccin-accentColor", "${config.catppuccin.accent}");
                '';
            inherit name;
          }
        ];
    };
    colorScheme = config.catppuccin.flavor;
    enabledExtensions = with spicePkgs.extensions; [
      # Official extensions
      keyboardShortcut
      shuffle
      # Community extensions
      seekSong
      goToSong
      skipStats
      songStats
      autoVolume
      history
      hidePodcasts
      adblock
      savePlaylists
      playNext
      volumePercentage
    ];
  };
}
