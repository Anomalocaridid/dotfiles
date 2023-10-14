{ config, pkgs, ... }: {
  programs.zathura = {
    enable = true;
    options =
      let
        fonts = config.stylix.fonts;
        theme = pkgs.custom.parseTheme {
          name = "zathura";
          themeFile = pkgs.fetchFromGitHub
            {
              owner = "catppuccin";
              repo = "zathura";
              rev = "d85d8750acd0b0247aa10e0653998180391110a4";
              hash = "sha256-5Vh2bVabuBluVCJm9vfdnjnk32CtsK7wGIWM5+XnacM=";
            } + "/src/catppuccin-${config.catppuccin.flavour}";
          keyField = 2;
          valueField = 3;
          formatStr = "\\\"%s\\\": %s";
        };
      in
      {
        # Copy to system clipboard
        selection-clipboard = "clipboard";
        font = "${fonts.sansSerif.name} ${toString fonts.sizes.applications}";
        window-title-basename = true;
        statusbar-home-tilde = true;
      } // theme;
  };
}
