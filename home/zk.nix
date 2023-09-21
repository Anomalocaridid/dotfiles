{ pkgs, ... }: {
  home.packages = with pkgs; [ zk ];
  xdg.configFile."zk/config.toml".source =
    let
      tomlFormat = pkgs.formats.toml { };
    in
    tomlFormat.generate "zk-config" {
      notebook.dir = "~/Sync/notes";

      note = {
        template = "default.md";
        filename = "{{slug title}}";
      };

      format.markdown = {
        link-format = "wiki";
        hashtags = true;
        colon-tags = true;
        multiword-tags = true;
      };

      # Need to specify the theme or else glow will not output color
      tool.fzf-preview = "glow --style ~/.config/glow/cyberpunk_neon.json {-1}";

      lsp.diagnostics = {
        dead-link = "error";
      };

      # filter = {};
      # aliases = {};
    };
}
