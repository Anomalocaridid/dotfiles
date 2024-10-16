{ pkgs, ... }:
{
  packages = with pkgs; [
    cabal-install # Package manager
    ghc # Compiler
    haskell-language-server
    hlint # Linter
    ormolu # Formatter
    stack # Package manager
  ];
}
