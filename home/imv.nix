{ config, pkgs, ... }: {
  programs.imv =
    let
      fonts = config.stylix.fonts;
      palette = pkgs.custom.catppuccin-palette.${config.catppuccin.flavour};
    in
    {
      enable = true;
      settings = {
        options = {
          background = "${palette.base.hex}";
          overlay = true;
          overlay_font = "${fonts.sansSerif.name}:${toString fonts.sizes.applications}";
          overlay_text = "$imv_current_file";
          overlay_text_color = "${palette.text.hex}";
          overlay_background_color = "${palette.mantle.hex}";
          title_text = "imv - \${imv_current_file##*/}";
        };
      };
    };
}
