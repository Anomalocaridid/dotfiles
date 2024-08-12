{ config, ... }:
{
  programs.imv =
    let
      fonts = config.stylix.fonts;
    in
    {
      enable = true;
      settings = {
        options = {
          overlay = true;
          overlay_font = "${fonts.sansSerif.name}:${toString fonts.sizes.applications}";
          overlay_text = "$imv_current_file";
          title_text = "imv - \${imv_current_file##*/}";
        };
      };
    };
}
