{ pkgs, ... }: {
  home.packages = with pkgs; [ imv ];

  xdg.configFile."imv/config".source =
    let
      iniFormat = pkgs.formats.ini { };
    in
    iniFormat.generate "imv-config" {
      options = {
        background = "000B1E";
        overlay = true;
        overlay_font = "Fira Code Nerd Font:12";
        overlay_text = "$imv_current_file";
        overlay_text_color = "0ABDC6";
        overlay_background_color = "091833";
        title_text = "imv - \${imv_current_file##*/}";
      };
      # aliases = { };
      # binds = { };
    };
}
