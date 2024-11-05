{ pkgs, inputs, ... }:
{
  packages = with pkgs; [
    (swi-prolog.override {
      extraPacks = map (dep-path: "'file://${dep-path}'") [
        inputs.swi-lsp-server
      ];
    })
  ];
}
