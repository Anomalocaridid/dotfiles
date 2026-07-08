{
  perSystem =
    { pkgs, ... }:
    {
      devshells.purescript.packages = with pkgs; [
        nodejs # Needed for exercism tests
        # FIXME: no longer in nixpkgs
        # purescript-language-server
        # purs-tidy # Code formatter
        purescript
        spago # Build tool
      ];
    };
}
