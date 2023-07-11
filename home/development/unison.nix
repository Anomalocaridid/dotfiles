{ pkgs, ... }: {
  home.packages = with pkgs; [
    unison-ucm
    netcat-gnu # required for lsp
  ];
}
