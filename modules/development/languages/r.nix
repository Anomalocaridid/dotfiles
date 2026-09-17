{
  perSystem = { pkgs, ... }: {
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

  flake.modules.homeManager.general.programs.helix.languages.language = [
    {
      name = "r";
      auto-format = true;
    }
  ];
}
