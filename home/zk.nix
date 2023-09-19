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

      tool = {
        fzf-preview = "bat --plain --color always {-1}";
        # Needed because using skim in place of fzf
        fzf-options = "--delimiter=' '";
      };

      lsp.diagnostics = {
        dead-link = "error";
      };

      # filter = {};
      # aliases = {};
    };
}
