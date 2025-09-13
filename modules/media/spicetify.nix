{ config, inputs, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  flake-file.inputs.spicetify-nix = {
    url = "github:Gerg-L/spicetify-nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  unify.modules.general = {
    nixos.environment.persistence.${persistDir}.users.${username}.directories = [
      ".cache/spotify" # Spotify cache
      ".config/spotify" # Spotify user data
    ];

    home =
      { config, pkgs, ... }:
      let
        spicePkgs = inputs.spicetify-nix.legacyPackages.${pkgs.system};
      in
      {
        imports = [ inputs.spicetify-nix.homeManagerModules.spicetify ];

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
            autoVolume
            history
            hidePodcasts
            adblock
            volumePercentage
            queueTime
            allOfArtist
          ];
        };
      };
  };
}
