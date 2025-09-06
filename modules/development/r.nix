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

  unify.modules.general.home.programs.helix.languages.language = [
    {
      name = "r";
      auto-format = true;
    }
  ];
}
