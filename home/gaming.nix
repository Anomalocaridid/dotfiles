{ config, lib, pkgs, inputs, ... }: {
  home.packages = with pkgs;
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
      (vinegar.overrideAttrs (oldAttrs:
        {
          buildInputs = map (x: if x.pname == "wine64-staging" then wine-ge else x) oldAttrs.buildInputs;

          postInstall = ''
            wrapProgram $out/bin/vinegar \
              --prefix PATH : ${lib.makeBinPath [wine-ge]}
          '';
        }))
    ];

  # Enable wine-ge's fsync support
  home.sessionVariables.WINEFSYNC = 1;

  ssbm.slippi-launcher = {
    enable = true;
    isoPath = "${config.home.homeDirectory}/Documents/Super Smash Bros. Melee (USA) (En,Ja) (Rev 2).ciso";
  };

  xdg =
    let
      palette = pkgs.custom.catppuccin-palette.${config.catppuccin.flavour};
    in
    {
      configFile = {
        # Cannot represent hex integers with generated toml
        "vinegar/config.toml".text = #toml
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
        "vinegar/overlay/content/sounds/ouch.ogg".source = pkgs.fetchurl {
          url = "https://archive.org/download/savetheoof/ouch.ogg";
          hash = "sha256-vcxbfljNxTFS+uFxrV/zKdDWWBBYN2hJIwkN5MN8AH0=";
        };
      };
      dataFile."PrismLauncher/themes/catppuccin-${config.catppuccin.flavour}".source =
        let
          capitalFlavour = (string:
            (lib.strings.toUpper (builtins.substring 0 1 string))
            + (builtins.substring 1 (builtins.stringLength string) string)
          ) config.catppuccin.flavour;
          themeDirectory = "share/catppuccin-${config.catppuccin.flavour}";
        in
        (pkgs.stdenvNoCC.mkDerivation rec {
          name = "catppuccin-prismlauncher-theme";
          version = "2023-10-17_1697558708";
          src = pkgs.fetchzip {
            url = "https://github.com/PrismLauncher/Themes/releases/download/${version}/Catppuccin-${capitalFlavour}.zip";
            hash = "sha256-QJ1pLSQFDn7p0d68MafSBp4cp5E2Bk5yqBqnOqdbu10=";
          };

          installPhase = ''
            runHook preInstall

            mkdir -p $out/${themeDirectory}
            cp -r $src/* $out/${themeDirectory}

            runHook postInstall
          '';

          # Inspired by https://github.com/catppuccin/prismlauncher/issues/1
          postInstall = ''
            substituteInPlace $out/${themeDirectory}/theme.json \
              --replace '"Highlight": "#b4befe"' '"Highlight": "#${palette.${config.catppuccin.accent}.hex}"'
          '';
        }) + "/${themeDirectory}";
    };
}
