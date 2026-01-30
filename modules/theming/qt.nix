{
  unify.modules.general.home =
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
        enable = true;
        platformTheme.name = "qtct";
        style.package = with pkgs; [
          darkly
          darkly-qt5
        ];
      }
      // (lib.genAttrs [ "qt5ctSettings" "qt6ctSettings" ] (_: {
        Appearance = {
          style = "Darkly";
          icon_theme = config.gtk.iconTheme.name;
        };
      }));
    };
}
