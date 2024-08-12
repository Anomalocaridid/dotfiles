{ pkgs, ... }:
{
  home.packages = with pkgs; [ glow ];

  xdg.configFile = {
    "glow/glow.yml".source =
      let
        yamlFormat = pkgs.formats.yaml { };
      in
      yamlFormat.generate "glow-format" {
        # show local files only; no network (TUI-mode only)
        local = true;
        # mouse support (TUI-mode only)
        mouse = true;
        # use pager to display markdown
        pager = true;
        # word-wrap at width
        width = 0;
      };
  };
}
