{
  perSystem = { pkgs, ... }: {
    devshells.clojure.packages = with pkgs; [
      clojure
      clojure-lsp
      leiningen # Needed for exercism tests
    ];
  };

  flake.modules.homeManager.general.programs.helix.languages.language = [
    {
      name = "clojure";
      auto-format = true;
    }
  ];
}
