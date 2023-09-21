{ pkgs, ... }: {
  home.packages = with pkgs; [ zk ];
  xdg.configFile."zk/config.toml".source =
    let
      tomlFormat = pkgs.formats.toml { };
      viewCommand = "glow --style ~/.config/glow/cyberpunk_neon.json";
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
      tool.fzf-preview = "${viewCommand} {-1}";

      lsp.diagnostics = {
        dead-link = "error";
      };

      # filter = {};

      alias = {
        show = "zk list --interactive --format \"{{abs-path}}\" | xargs zsh -c 'PAGER=\"bat --file-name $0\" ${viewCommand} $0'";
      };
    };
}

