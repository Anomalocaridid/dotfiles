{ config, pkgs, ... }: {
  home.packages = with pkgs; [
    glow
  ];

  programs.glamour.catppuccin.enable = true;

  xdg.configFile =
    let
      glowThemePath = "glow/catppuccin-${config.catppuccin.flavour}.json";
    in
    {
      "glow/glow.yml".source =
        let
          yamlFormat = pkgs.formats.yaml { };
        in
        yamlFormat.generate "glow-format" {
          # style name or JSON path (default "auto")
          # style = "~/.config/${glowThemePath}";
          # show local files only; no network (TUI-mode only)
          local = true;
          # mouse support (TUI-mode only)
          mouse = true;
          # use pager to display markdown
          pager = true;
          # word-wrap at width
          width = 0;
        };
      # "${glowThemePath}".source = pkgs.fetchurl {
      #   url = "https://github.com/catppuccin/glamour/releases/download/v1.0.0/${config.catppuccin.flavour}.json";
      #   hash = "sha256-Tx2fQteL4wxhV+qHYZibakiYoEhS4HjyMO0yBcU/F6Q=";
      # };
    };
}
