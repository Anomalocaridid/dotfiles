{
  perSystem =
    { pkgs, ... }:
    {
      devshells.elm.packages = with pkgs.elmPackages; [
        elm
        elm-test # Diagnostics and running tests
        elm-format
        elm-language-server
      ];
    };
}
