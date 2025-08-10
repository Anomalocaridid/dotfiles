{
  perSystem =
    { pkgs, ... }:
    {
      devshells.clojure.packages = with pkgs; [
        clojure
        clojure-lsp
        leiningen # Needed for exercism tests
      ];
    };
}
