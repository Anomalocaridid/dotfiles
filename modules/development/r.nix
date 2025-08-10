{
  perSystem =
    { pkgs, ... }:
    {
      devshells.r.packages = with pkgs; [
        (rWrapper.override {
          packages = with rPackages; [
            languageserver
            lintr
            testthat # Needed for exercism tests
          ];
        })
      ];
    };
}
