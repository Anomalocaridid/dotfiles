{ config, pkgs, ... }: {
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
        theme = pkgs.fetchFromGitHub {
          owner = "quantumfate";
          repo = "wofi";
          rev = "6c37e0f65b9af45ebe680e3e0f5131f452747c6f";
          hash = "sha256-zQGiF/8WZ15ZlQVVgxuQq4qatinxMx2Y6Xl5Zcuhp7Y=";
        };
      in
      builtins.readFile
        (pkgs.runCommand "catppuccin-wofi-theme" { }
          ''
            sed 's/Inconsolata Nerd Font/${fonts.sansSerif.name}/' <${theme}/src/${config.catppuccin.flavour}/style.css > $out
          '');
  };
}
