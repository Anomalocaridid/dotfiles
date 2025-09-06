{ inputs, ... }:
{
  unify.modules.general.home =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [ glow ];

      xdg.configFile."glow/glow.yml".source =
        (inputs.nixago.lib.${pkgs.system}.make {
          data = {
            # show local files only; no network (TUI-mode only)
            local = true;
            # mouse support (TUI-mode only)
            mouse = true;
            # use pager to display markdown
            pager = true;
            # word-wrap at width
            width = 0;
          };
          output = "glow.yml";
          format = "yaml";
        }).configFile;
    };
}
