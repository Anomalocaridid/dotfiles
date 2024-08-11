{ pkgs, ... }:
{
  home.packages =
    with pkgs;
    [
      dune_3 # package manager
      dune-release
      ocaml
    ]
    ++ (with ocamlPackages; [
      findlib # needed for everything to work
      odoc # document generator
      ounit2 # unit test runner
      utop # repl
    ]);

  programs.helix.extraPackages = with pkgs; [
    ocamlformat
    ocamlPackages.ocaml-lsp
  ];
}
