{
  unify.modules.general.home =
    {
      config,
      lib,
      pkgs,
      osConfig,
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
        Appearance.style = "Darkly";
      }));
    };
}
