{ pkgs, ... }:
{
  packages = with pkgs; [
    nodejs # Needed for exercism tests
    nodePackages.purescript-language-server
    nodePackages.purs-tidy # Code formatter
    purescript
    spago # Build tool
  ];
}
