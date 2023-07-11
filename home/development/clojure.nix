{ pkgs, ... }: {
  home.packages = with pkgs; [
    clojure
    clojure-lsp
    leiningen
  ];
}
