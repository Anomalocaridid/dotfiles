{ pkgs, ... }:
{
  packages = with pkgs; [
    clojure
    clojure-lsp
    leiningen # Needed for exercism tests
  ];
}
