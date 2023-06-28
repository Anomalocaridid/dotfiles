final: prev: {
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
  };
}
