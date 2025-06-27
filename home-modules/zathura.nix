{ ... }:
{
  programs.zathura = {
    enable = true;
    options = {
      recolor = false;
      selection-clipboard = "clipboard";
      statusbar-home-tilde = true;
      window-title-basename = true;
    };
  };
}
