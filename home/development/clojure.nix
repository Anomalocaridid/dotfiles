{ pkgs, ... }:
{
  home.packages = with pkgs; [
    clojure
    leiningen # Needed for exercism tests
  ];

  programs.helix = {
    extraPackages = with pkgs; [ clojure-lsp ];
    languages.language = [
      {
        name = "clojure";
        auto-format = true;
      }
    ];
  };
}
