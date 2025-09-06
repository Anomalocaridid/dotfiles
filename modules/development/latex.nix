{
  perSystem =
    { pkgs, ... }:
    {
      devshells.latex.packages = with pkgs; [
        tectonic # latex engine
        texlab # latex lsp
      ];
    };

  unify.modules.general.home =
    { lib, pkgs, ... }:
    {
      # NOTE: only works for standalone LaTeX files, not Tectonic projects
      programs.helix.languages.language-server.texlab.config.texlab = {
        build = {
          onSave = true;
          executable = "tectonic";
          args = [
            "-X"
            "compile"
            "%f"
            "--synctex"
            "--keep-logs"
            "--keep-intermediates"
          ];
          forwardSearchAfter = true;
        };
        forwardSearch = {
          executable = "sioyek";
          args = [
            "--reuse-window"
            "--execute-command"
            "toggle_synctex"
            "--inverse-search"
            ''texlab inverse-search --input "%%1" --line %%2''
            "--forward-search-file"
            "%f"
            "--forward-search-line"
            "%l"
            "%p"
          ];
        };
      };
    };
}
