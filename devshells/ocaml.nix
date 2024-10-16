{ pkgs, ... }:
{
  packages =
    with pkgs;
    [
      dune_3 # package manager
      dune-release
      ocaml
      ocamlformat # code formatter
      ocamlPackages.ocaml-lsp
    ]
    ++ (with ocamlPackages; [
      findlib # needed for everything to work
      odoc # document generator
      ounit2 # unit test runner
      utop # repl
    ]);
}
