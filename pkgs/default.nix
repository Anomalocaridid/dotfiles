final: prev: {
  # Customized packages
  sudo = prev.sudo.override {
    withInsults = true;
  };

  nnn = (prev.nnn.override {
    withNerdIcons = true;
  }).overrideAttrs
    (oldAttrs: {
      # Required for O_PCRE=1
      buildInputs = (oldAttrs.buildInputs or [ ]) ++ [ final.pcre ];
      # Custom compile options
      makeFlags = (oldAttrs.makeFlags or [ ]) ++ [
        "O_PCRE=1" # Enable PCRE regex support
        "O_NOBATCH=1" # Disable built-in batch renamed in favor of .nmv plugin
        "O_NOUG=1" # Don't display user or group info in status bar to save memory
        "O_QSORT=1" # Use Alexey Tourbin's QSORT
        "O_CTX8=1" # Use 8 contexts instead of 4
        # Patches
        "O_GITSTATUS=1" # Show git status in detail mode (and normal mode with -G)
        "O_NAMEFIRST=1" # Print filenames first in detail mode
      ];
      postInstall = (oldAttrs.postInstall or "") + ''
        # Install plugins
        mkdir -p $out/share/plugins
        install -D -m755 plugins/* $out/share/plugins
        # Install quitcd
        mkdir -p $out/share/quitcd
        install -D -m644 misc/quitcd/* $out/share/quitcd
      '';
    });

  zsh = prev.zsh.overrideAttrs (oldAttrs: {
    patches = (oldAttrs.patches or [ ]) ++ [
      # Patch to resolve issues with reflowing text when changing window dimensions
      (final.fetchpatch {
        url = "https://github.com/zsh-users/zsh/compare/master...romkatv:zsh:fix-winchanged.patch";
        hash = "sha256-TwQv8c0pa7gI7DI5rWhwLyl2aQGwILQgd2V5Zem53uQ=";
      })
    ];
  });

  # Custom-written packages
  custom = {
    catppuccin-palette-files = final.callPackage ./catppuccin-palette-files { };
    catppuccin-palette = (builtins.fromJSON (builtins.readFile (final.custom.catppuccin-palette-files + /share/palette-porcelain.json)));
    candy-icons = final.callPackage ./candy-icons { };
    # cp and mv with progress bars
    advcpmv-coreutils = prev.coreutils.overrideAttrs (oldAttrs: rec {
      advcpmv-patch = final.fetchpatch {
        url = "https://raw.githubusercontent.com/jarun/advcpmv/master/advcpmv-0.9-9.3.patch";
        hash = "sha256-I25F7uHESUsMDZFYTv8/56eR8QwelIPpABRXTgvszQI=";
      };

      patches = (oldAttrs.patches or [ ]) ++ [ advcpmv-patch ];
    });
    # custom locking script
    lockman = final.callPackage ./lockman { };
    # custom screenshot script
    screenshot = final.callPackage ./screenshot { };
  };
}




