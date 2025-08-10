{ inputs, ... }:
{
  flake.modules.homeManager.spicetify =
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
}
