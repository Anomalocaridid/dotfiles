{ lib, pkgs, ... }: {
  programs.nnn = rec {
    enable = true;
    bookmarks = {
      d = "~/Documents";
      D = "~/Downloads";
      e = "~/exercism";
      n = "~/nixos";
      p = "~/Pictures";
      v = "~/Videos";
    };
    plugins = {
      mappings = {
        c = "chksum";
        d = "dragdrop";
        M = "nmount";
        n = "bulknew";
        s = "!zsh -i";
        z = "autojump";
      };
      src = "${package}/share/plugins";
    };
    extraPackages =
      with pkgs;
      let
        wrapAdvcpmv = (util:
          writeShellApplication {
            name = util + "g";
            runtimeInputs = [ custom.advcpmv-coreutils ];
            text = ''
              ${util} "$@"
            '';
          }
        );
      in
      [
        pmount
        udisks
        xdragon
        custom.advcpmv-coreutils # add progress bars to cp and mv (depends on overlay in flake.nix)
      ] ++ (map wrapAdvcpmv [ "cp" "mv" ]); # nnn support for advcpmv-coreutils
    package = (pkgs.nnn.override {
      withNerdIcons = true;
    }).overrideAttrs
      (oldAttrs: {
        # Required for O_PCRE=1
        buildInputs = (oldAttrs.buildInputs or [ ]) ++ [ pkgs.pcre ];
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
        postInstall = lib.concatStrings [
          (oldAttrs.postInstall or "")
          ''
            # Install desktop file
            make PREFIX=${placeholder "out"} install-desktop
            # Install plugins
            mkdir -p $out/share/plugins
            install -D -m755 plugins/* $out/share/plugins
            # Install quitcd
            mkdir -p $out/share/quitcd
            install -D -m644 misc/quitcd/* $out/share/quitcd
          ''
        ];
      });
  };

  home.sessionVariables = {
    NNN_OPTS = "cDEirx";
    NNN_OPENER = "$HOME/.config/nnn/plugins/nuke";
    GUI = 1;
    NNN_FIFO = "/tmp/nnn.fifo";
    NNN_COLORS = "#1909c9d02e0d2cff";
  };
}
