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

  unify.modules.development.home.programs.helix.languages.language = [
    {
      name = "clojure";
      auto-format = true;
    }
  ];
}
