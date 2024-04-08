final: prev: {
  # Customized packages
  # Remove once $LESSOPEN support is enabled by default
  bat = prev.bat.overrideAttrs (oldAttrs: rec {
    cargoBuildFeatures = (oldAttrs.cargoBuildFeatures or [ ]) ++ [ "lessopen" ];
    # Remove once bat#2805 is merged and bat has a new release
    version = "unstable-2023-12-18";
    src = final.fetchFromGitHub {
      owner = "Anomalocaridid";
      repo = oldAttrs.pname;
      rev = "11e40104b4f85cb4da04f3e2fab3ec76ae673aeb";
      hash = "sha256-kWpqTEqiZuI4j1tG/fk+Xw252O2ryrNdZqjbqCRYCrk=";
    };
    cargoDeps = oldAttrs.cargoDeps.overrideAttrs (oldDeps: {
      inherit src;
      lockFile = "${src}/Cargo.lock";
      outputHash = "sha256-WAfCEbhJXvWOvALhCg0QCmKtI3lb9rvTyywqwpCKHWY=";
    });
  });

  # Custom-written packages
  custom = {
    catppuccin-palette-files = final.callPackage ./catppuccin-palette-files { };
    catppuccin-palette = (builtins.fromJSON (builtins.readFile (final.custom.catppuccin-palette-files + /share/palette-porcelain.json)));
    # custom locking script
    lockman = final.callPackage ./lockman { };
    # custom screenshot script
    screenshot = final.callPackage ./screenshot { };
  };

  # Generated sources
  sources = builtins.mapAttrs (_: p: p.src)
    ((import ../_sources/generated.nix) {
      inherit (final) fetchurl fetchgit fetchFromGitHub dockerTools;
    });
}




