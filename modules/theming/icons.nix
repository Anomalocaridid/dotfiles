{
  unify.modules.general = {
    nixos =
      { pkgs, ... }:
      {
        # Allow svg icons in various places like wlogout and xdragon
        programs.gdk-pixbuf.modulePackages = with pkgs; [ librsvg ];
      };

    home =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        # Disable Catppuccin icons because I want to use a different theme
        catppuccin.gtk.icon.enable = false;

        # fallback icon theme
        home.packages = with pkgs; [ adwaita-icon-theme ];

        gtk.iconTheme = {
          name = "candy-icons";
          # Merge Candy Icons and Sweet Folders into the same package and recolor
          package =
            pkgs.runCommand "recolored-icons"
              {
                nativeBuildInputs = with pkgs; [ lutgen ];
                OUT_DIR = "${placeholder "out"}/share/icons";
              }
              ''
                mkdir --parents "$OUT_DIR/places"
                cp --recursive --no-preserve=mode ${pkgs.candy-icons}/share/icons/candy-icons "$OUT_DIR"
                cp --recursive --no-preserve=mode ${pkgs.sweet-folders}/share/icons/Sweet-Rainbow/Places/* "$OUT_DIR/places"
                # Remove broken symlink
                rm "$OUT_DIR/candy-icons/places/16/folder-library.svg"
                lutgen patch --write --no-patch --palette="catppuccin-${config.catppuccin.flavor}" --nearest-neighbor $(find "$OUT_DIR" -name '*.svg')
              '';
        };
      };
  };
}
