{ pkgs, inputs, ... }:
{
  packages = with pkgs; [
    nodejs # Needed for exercism tests
    nodePackages.purescript-language-server
    nodePackages.purs-tidy # Code formatter
    # purescript
    # Most recent version in nixpkgs is too far ahead for exercism
    # TODO: remove when exercism's purescript track updates
    inputs.purescript-overlay.packages.${pkgs.system}.purs-0_14_7
    # spago # Build tool
    # Most recent version in nixpkgs has a bug
    # TODO: remove when spago in nixpkgs updates
    inputs.purescript-overlay.packages.${pkgs.system}.spago-0_20_9 # Build tool
  ];
}
