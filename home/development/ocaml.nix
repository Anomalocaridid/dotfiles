{ pkgs, ... }: {
  home.packages = with pkgs; [
    dune_3 # package manager
    ocamlformat
    dune-release
  ] ++ (with ocamlPackages; [
    ocaml-lsp
    odoc # document generator
    utop # repl
    ounit # unit test runner
  ]);
}
