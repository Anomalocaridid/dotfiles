{ pkgs, inputs, ... }:
let
  spicePkgs = inputs.spicetify-nix.packages.${pkgs.system}.default;
in
{
  # Needs to be imported here in home manager config, not in flake.nix
  imports = [ inputs.spicetify-nix.homeManagerModules.spicetify ];

  programs.spicetify = {
    enable = true;
    enabledExtensions = with spicePkgs.extensions; [
      keyboardShortcut
      shuffle
      hidePodcasts
      seekSong
      adblock
      playNext
      volumePercentage
    ];

    colorScheme = "custom";

    customColorScheme =
      let
        dark = "000b1e";
        blue = "091833";
        light-blue = "133e7c";
        cyan = "0abdc6";
        pink = "ea00d9";
        red = "ff0000";
        green = "00ff00";
      in
      {
        text = cyan;
        subtext = pink;
        sidebar-text = cyan;
        main = dark;
        sidebar = dark;
        player = dark;
        card = blue;
        shadow = dark;
        selected-row = pink;
        button = light-blue;
        button-active = cyan;
        button-disabled = light-blue;
        tab-active = pink;
        notification = pink;
        notification-error = red;
        # idk what this goes to, but it should be obvious if I find it
        misc = green;
      };
  };
}
