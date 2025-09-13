{ inputs, ... }:
{
  # Prolog language server
  flake-file.inputs.swi-lsp-server = {
    url = "github:jamesnvc/lsp_server";
    flake = false;
  };

  perSystem =
    { pkgs, ... }:
    {
      devshells.prolog.packages = with pkgs; [
        (swi-prolog.override {
          extraPacks = map (dep-path: "'file://${dep-path}'") [
            inputs.swi-lsp-server
          ];
        })
      ];
    };
}
