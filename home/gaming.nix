{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
{
  home.packages =
    with pkgs;
    let
      wine-ge = inputs.nix-gaming.packages.${pkgs.system}.wine-ge;
    in
    [
      gamescope # Used by Lutris for control over game resolution
      lutris
      packwiz # minecraft modpack creator
      parsec-bin # Online multiplayer for local multiplayer games
      prismlauncher
      pysolfc
      runelite
      sgt-puzzles
      wine-ge # System-level install for Lutris
      # Use custom wine build
      # Also prevents build failures if there are issues with patch
      (vinegar.overrideAttrs (oldAttrs: {
        buildInputs = map (x: if x.pname == "wine64-staging" then wine-ge else x) oldAttrs.buildInputs;

        postInstall = ''
          wrapProgram $out/bin/vinegar \
            --prefix PATH : ${lib.makeBinPath [ wine-ge ]}
        '';
      }))
    ];

  # Enable wine-ge's fsync support
  home.sessionVariables.WINEFSYNC = 1;

  ssbm.slippi-launcher = {
    # enable = true;
    isoPath = "${config.home.homeDirectory}/Documents/Super Smash Bros. Melee (USA) (En,Ja) (Rev 2).ciso";
  };

  xdg =
    let
      palette = pkgs.custom.catppuccin-palette.${config.catppuccin.flavour};
    in
    {
      configFile = {
        # Cannot represent hex integers with generated toml
        "vinegar/config.toml".text = # toml
          ''
            # Customize splash screen
            [splash]
            background = 0x${palette.base.hex}
            foreground = 0x${palette.text.hex}
            cancel = 0x${palette.red.hex}
            accent = 0x${palette.${config.catppuccin.accent}.hex}
            track = 0x${palette.surface0.hex}
            info = 0x${palette.yellow.hex}
          '';
        # Use the classic Roblox oof
        "vinegar/overlay/content/sounds/ouch.ogg".source = pkgs.sources.roblox-oof;
      };
      dataFile."PrismLauncher/themes/catppuccin-${config.catppuccin.flavour}".source =
        let
          capitalFlavour =
            (
              string:
              (lib.strings.toUpper (builtins.substring 0 1 string))
              + (builtins.substring 1 (builtins.stringLength string) string)
            )
              config.catppuccin.flavour;
        in
        pkgs.runCommand "catppuccin-prismlauncher-theme" { } ''
          mkdir -p $out
          cp -r ${pkgs.sources.catppuccin-prismlauncher}/themes/${capitalFlavour}/* $out
          substituteInPlace $out/theme.json \
            --replace '"Highlight": "#b4befe"' '"Highlight": "#${palette.${config.catppuccin.accent}.hex}"'
        '';
    };
}
