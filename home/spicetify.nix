{ pkgs, inputs, config, ... }:
let
  spicePkgs = inputs.spicetify-nix.packages.${pkgs.system}.default;
in
{
  # Needs to be imported here in home manager config, not in flake.nix
  imports = [ inputs.spicetify-nix.homeManagerModules.spicetify ];

  programs.spicetify = {
    enable = true;
    theme = spicePkgs.themes."catppuccin-${config.catppuccin.flavour}";
    colorScheme = config.catppuccin.accent;
    enabledExtensions = with spicePkgs.extensions; [
      keyboardShortcut
      shuffle
      hidePodcasts
      seekSong
      adblock
      playNext
      volumePercentage
    ];
  };
}
