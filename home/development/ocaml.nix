{ pkgs, ... }: {
  home.packages = with pkgs; [
    dune_3 # package manager
    dune-release
    ocaml
    ocamlformat
  ] ++ (with ocamlPackages; [
    findlib # needed for everything to work
    ocaml-lsp
    odoc # document generator
    ounit2 # unit test runner
    utop # repl
  ]);
}
