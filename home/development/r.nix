{ pkgs, ... }:
{
  home.packages = with pkgs; [
    (rWrapper.override {
      packages = with rPackages; [
        testthat # Needed for exercism tests
      ];
    })
  ];

  programs.helix = {
    extraPackages = with pkgs; [
      (rWrapper.override {
        packages = with rPackages; [
          languageserver
          lintr
        ];
      })
    ];
    languages.language = [
      {
        name = "r";
        auto-format = true;
      }
    ];
  };
}
