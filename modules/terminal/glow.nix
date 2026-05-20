{
  unify.modules.general.home =
    { config, pkgs, ... }:
    {
      home.packages = with pkgs; [ glow ];

      xdg.configFile."glow/glow.yml".source = pkgs.writers.writeYAML "glow.yml" {
        # Set theme
        style = config.home.sessionVariables.GLAMOUR_STYLE;
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
