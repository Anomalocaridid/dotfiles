{ pkgs, inputs, ... }:
{
  packages = with pkgs; [
    nodejs # Needed for exercism tests
    nodePackages.purescript-language-server
    nodePackages.purs-tidy # Code formatter
    purescript
    # spago # Build tool
    # Most recent version in nixpkgs has a bug
    inputs.purescript-overlay.packages.${pkgs.system}.spago-0_20_9 # Build tool
  ];
}
