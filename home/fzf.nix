{ config, pkgs, ... }: {
  programs.fzf = {
    enable = true;
    colors =
      let
        palette = pkgs.custom.catppuccin-palette.${config.catppuccin.flavour};
      in
      {
        "bg+" = "#${palette.surface0.hex}";
        bg = "#${palette.base.hex}";
        spinner = "#${palette.rosewater.hex}";
        hl = "#${palette.red.hex}";
        fg = "#${palette.text.hex}";
        header = "#${palette.red.hex}";
        info = "#${palette.mauve.hex}";
        pointer = "#${palette.rosewater.hex}";
        marker = "#${palette.rosewater.hex}";
        "fg+" = "#${palette.text.hex}";
        prompt = "#${palette.mauve.hex}";
        "hl+" = "#${palette.red.hex}";
      };
    defaultOptions = [ ''--preview 'bat --plain --color=always "{}"' '' ];
  };
}




