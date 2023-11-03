{ config, pkgs, inputs, ... }: {
  programs.wofi = {
    enable = true;
    settings = {
      hide_scroll = true;
      show = "drun";
      width = "30%";
      lines = 10;
      line_wrap = "word";
      term = "handlr launch x-scheme-handler/terminal";
      allow_markup = true;
      always_parse_args = true;
      show_all = true;
      print_command = true;
      layer = "overlay";
      allow_images = true;
      insensitive = true;
      prompt = "";
      image_size = 15;
      display_generic = true;
      location = "center";
    };
    style =
      let
        fonts = config.stylix.fonts;
      in
      builtins.readFile
        (pkgs.runCommand "catppuccin-wofi-theme" { }
          ''
            cp ${inputs.catppuccin-wofi + /src/${config.catppuccin.flavour}/style.css} $out
            substituteInPlace $out --replace "Inconsolata Nerd Font" "${fonts.monospace.name}";
          '');
  };
}
