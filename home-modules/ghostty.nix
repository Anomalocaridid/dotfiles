{ config, ... }:
{
  programs.ghostty =
    let
      fonts = config.stylix.fonts;
    in
    {
      enable = true;
      settings = {
        font-family = fonts.monospace.name;
        font-size = fonts.sizes.terminal;
        theme = "catppuccin-${config.catppuccin.flavor}";
        window-decoration = false;
        background-opacity = 0.9;
        window-padding-color = "extend";
        confirm-close-surface = false;
        font-feature = [
          "ss09" # >>= <<= ||= |=
          "cv25" # .-
          "cv26" # :-
          "cv32" # .=
          "cv27" # []
          "cv28" # {. .}
          "ss06" # \\
          "ss07" # =~ !~
        ];
      };
    };
}
