{ ... }:
{
  programs.helix.languages = {
    language-server.zk = {
      command = "zk"; # Already installed for other things
      args = [ "lsp" ];
    };
    language = [
      {
        name = "markdown";
        auto-format = true;
        indent = {
          tab-width = 4;
          unit = "    ";
        };
        # Use zk instead of default lsp
        roots = [ ".zk" ];
        language-servers = [ "zk" ];
      }
    ];
  };
}
