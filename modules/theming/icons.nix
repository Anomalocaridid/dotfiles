{
  unify.modules.general = {
    nixos =
      { pkgs, ... }:
      {
        # Allow svg icons in various places like wlogout and xdragon
        programs.gdk-pixbuf.modulePackages = with pkgs; [ librsvg ];
      };

    home =
      { config, pkgs, ... }:
      {
        # fallback icon theme
        home.packages = with pkgs; [ adwaita-icon-theme ];

        gtk.iconTheme =
          let
            recolor-icons =
              pkgs.writers.writePython3 "recolor-icons" { libraries = [ pkgs.custom.color-manager ]; }
                ''
                  import sys
                  from color_manager import utils

                  src = sys.argv[1]
                  dest = sys.argv[2]
                  name = "candy-icons"
                  palettes = "${pkgs.custom.color-manager.src}/palettes/"
                  palette = palettes + "catppuccin_${config.catppuccin.flavor}.json"

                  utils.recolor(src, dest, name, palette)
                '';
          in
          {
            name = "candy-icons";
            # Merge Candy Icons and Sweet Folders into the same package and recolor
            package = pkgs.runCommand "recolored-icons" { } ''
              cp --recursive --no-preserve=mode ${pkgs.candy-icons}/share/icons/candy-icons tmp
              cp --recursive --no-preserve=mode ${pkgs.sweet-folders}/share/icons/Sweet-Rainbow/Places/16/* tmp/places/16
              cp --recursive --no-preserve=mode ${pkgs.sweet-folders}/share/icons/Sweet-Rainbow/Places/48/* tmp/places/48
              mkdir --parents $out/share/icons
              ${recolor-icons} tmp $out/share/icons
            '';
          };
      };
  };
}
