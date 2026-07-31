{
  unify = {
    modules.general.home.qt.enable = true;

    home =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        catppuccin = {
          kvantum.assertStyle = false;
          qt5ct.enable = true;
        };

        qt = {
          platformTheme.name = "qtct";
          style.package = with pkgs; [ darkly ];
        }
        // (lib.genAttrs [ "qt5ctSettings" "qt6ctSettings" ] (_: {
          Appearance = {
            style = "Darkly";
            icon_theme = config.gtk.iconTheme.name;
          };
        }));
      };
  };
}
