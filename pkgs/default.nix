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

  # Fix crash when opening with current version of Hyprland
  # Remove once WezTerm has a new release and updates
  wezterm = prev.wezterm.overrideAttrs (oldAttrs: rec {
    version = "unstable-2023-11-23";
    src = final.fetchFromGitHub {
      owner = "wez";
      repo = oldAttrs.pname;
      rev = "6a58a5ce94f186884ec70a60b5afbd728521b1c5";
      fetchSubmodules = true;
      hash = "sha256-QXZjGIw5LvK+frigdCYGVOjLHM3Fnnqqi5FEySaKExs=";
    };
    cargoDeps = final.rustPlatform.importCargoLock {
      lockFile = "${src}/Cargo.lock";
      outputHashes = {
        "xcb-1.2.1" = "sha256-zkuW5ATix3WXBAj2hzum1MJ5JTX3+uVQ01R1vL6F1rY=";
        "xcb-imdkit-0.2.0" = "sha256-L+NKD0rsCk9bFABQF4FZi9YoqBHr4VAZeKAWgsaAegw=";
      };
    };
  });

  # Add tray with dynamic icon support
  # Remove once elkowar#743 and ralismark#4 are merged
  eww-wayland = prev.eww-wayland.overrideAttrs (oldAttrs: rec {
    patches = (oldAttrs.patches or [ ]) ++ [
      # Don't use fetchpatch because it somehow makes it not find certain files
      # This patch should include both elkowar#743 and ralismark#4
      (final.fetchurl {
        url = "https://github.com/elkowar/eww/compare/${oldAttrs.src.rev}...hylophile:eww:dynamic-icons.patch";
        hash = "sha256-aQgK+46N9H83li4FAj8+vw0ntZW2vUke5Ovj0FneVi0=";
      })
    ];
    cargoDeps = oldAttrs.cargoDeps.overrideAttrs (oldDeps: {
      patches = (oldDeps.patches or [ ]) ++ patches;
      outputHash = "sha256-3B81cTIVt/cne6I/gKBgX4zR5w0UU60ccrFGV1nNCoA=";
    });
    buildInputs = (oldAttrs.buildInputs or [ ]) ++ [
      final.libdbusmenu
      final.libdbusmenu-gtk3
    ];
  });

  # Custom-written packages
  custom = {
    catppuccin-palette-files = final.callPackage ./catppuccin-palette-files { };
    catppuccin-palette = (builtins.fromJSON (builtins.readFile (final.custom.catppuccin-palette-files + /share/palette-porcelain.json)));
    candy-icons = final.callPackage ./candy-icons { };
    breeze-hacked-cursor = final.callPackage ./breeze-hacked-cursor { };
    # custom locking script
    lockman = final.callPackage ./lockman { };
    # custom screenshot script
    screenshot = final.callPackage ./screenshot { };
  };
}




