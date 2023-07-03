final: prev: {
  # Customized packages
  sudo = prev.sudo.override {
    withInsults = true;
  };

  nerdfonts = prev.nerdfonts.override { fonts = [ "FiraCode" ]; };

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

  # Custom-written packages
  custom = {
    cyberre-grub-theme = final.pkgs.callPackage ./cyberre-grub-theme { };
    gtk-materia-cyberpunk-neon = final.pkgs.callPackage ./gtk-materia-cyberpunk-neon { };
    candy-icons = final.pkgs.callPackage ./candy-icons { };
    breeze-hacked-cursor = final.pkgs.callPackage ./breeze-hacked-cursor { };
    # cp and mv with progress bars
    advcpmv-coreutils = prev.coreutils.overrideAttrs (oldAttrs: rec {
      version = "9.2";
      patch-version = "0.9";

      # Necessary because advcpmv can lag behind coreutils
      src = final.fetchurl {
        url = "mirror://gnu/coreutils/coreutils-${version}.tar.xz";
        hash = "sha256-aIX/R7nNshHeR9NowXhT9Abar5ixSKrs3xDeKcwEsLM=";
      };

      advcpmv-patch = final.fetchpatch {
        url = "https://raw.githubusercontent.com/jarun/advcpmv/master/advcpmv-${patch-version}-${version}.patch";
        hash = "sha256-l2WGdEKd8hxHUL3IrK+8jFuc0eLme0387WB7ObWWwIA=";
      };

      patches = (oldAttrs.patches or [ ]) ++ [ advcpmv-patch ];
    });
    # custom locking script
    lockman = (final.writeShellApplication {
      name = "lockman.sh";
      runtimeInputs = with final; [
        coreutils
        handlr-regex
        hyprland
        pipes-rs
        swaylock-effects
        wezterm
      ];
      text = ''
        # NOTE: screensaver gets closed externally by swayidle
        # Move to empty workspace and run screensaver
        hyprctl dispatch workspace empty
        handlr launch x-scheme-handler/terminal -- --class=lockman -- pipes-rs
        # Fullscreen screensaver
        sleep 0.1 # Slight delay to ensure screensaver is focused
        hyprctl dispatch fullscreen 0
        # Lock screen (blocks until unlocked)
        swaylock
        # Close screensaver
        hyprctl --batch "dispatch closewindow ^(lockman)$; dispatch workspace previous"
      '';
    });
  };
}
