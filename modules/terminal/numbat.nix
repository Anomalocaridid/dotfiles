{
  unify.modules.general.home =
    { pkgs, ... }:
    {
      programs.numbat = {
        enable = true;
        # TODO: remove when the desktop file and icons are added upstream
        package = pkgs.numbat.overrideAttrs (oldAttrs: {
          postInstall = (oldAttrs.postInstall) + ''
            mkdir -p $out/share/applications
            cp $src/assets/numbat.desktop $out/share/applications
            mkdir -p $out/share/icons/hicolor/scalable/apps/
            cp $src/assets/numbat.svg $out/share/icons/hicolor/scalable/apps
          '';
        });
        settings = {
          exchange-rates.fetching-policy = "on-first-use";
        };
      };
    };
}
