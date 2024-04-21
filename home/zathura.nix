{ config, ... }:
{
  programs.zathura = {
    enable = true;
    catppuccin.enable = true;
    options =
      let
        fonts = config.stylix.fonts;
      in
      {
        # Copy to system clipboard
        selection-clipboard = "clipboard";
        font = "${fonts.sansSerif.name} ${toString fonts.sizes.applications}";
        window-title-basename = true;
        statusbar-home-tilde = true;
      };
  };
}
