{ config, pkgs, ... }: {
  services.mako =
    let
      fonts = config.stylix.fonts;
    in
    {
      enable = true;
      font = "${fonts.sansSerif.name} ${toString fonts.sizes.popups}";
      width = 315;
      height = 200;
      padding = "10";
      margin = "10";
      iconPath = "${config.gtk.iconTheme.package}";
      maxIconSize = 70;
      layer = "overlay";
      borderSize = 5;
      borderRadius = 5;
      defaultTimeout = 5000;
      extraConfig = builtins.readFile (pkgs.fetchFromGitHub
        {
          owner = "catppuccin";
          repo = "mako";
          rev = "9dd088aa5f4529a3dd4d9760415e340664cb86df";
          hash = "sha256-nUzWkQVsIH4rrCFSP87mXAka6P+Td2ifNbTuP7NM/SQ=";
        } + /src/${config.catppuccin.flavour});
    };
}
