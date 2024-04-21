{
  config,
  pkgs,
  inputs,
  ...
}:
let
  spicePkgs = inputs.spicetify-nix.packages.${pkgs.system}.default;
in
{
  programs.spicetify = {
    enable = true;
    # Use pre-refactor (pre-#48) catppuccin theme
    # so that the accent can be declaratively defined
    theme = spicePkgs.themes.catppuccin // rec {
      name = "catppuccin-${config.catppuccin.flavour}";
      src = pkgs.fetchFromGitHub {
        owner = "catppuccin";
        repo = "spicetify";
        rev = "0b602142da4a436f7012c07137e846034aea82cb";
        hash = "sha256-dBJ0vLvCdzpZZNHrsY6V8JQ2t4RB2L5OF/MdHGmIk4Y=";
      };
      requiredExtensions = [
        {
          src = "${src}/js";
          filename = "${name}.js";
        }
      ];
    };
    colorScheme = config.catppuccin.accent;
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
