{ ... }:
{
  programs.imv = {
    enable = true;
    settings = {
      options = {
        overlay = true;
        overlay_text = "$imv_current_file";
        title_text = "imv - \${imv_current_file##*/}";
      };
    };
  };
}
