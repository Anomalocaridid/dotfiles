{
  perSystem =
    { pkgs, inputs', ... }:
    {
      devshells.python.packages = with pkgs; [
        (python3.withPackages (
          ps: with ps; [
            pytest # Needed for exercism tests
            (inputs'.ignis.packages.ignis.override {
              extraPackages = with pkgs.python313Packages; [
                psutil
                unicodeit
              ];
            })
          ]
        ))
        basedpyright # Python typechecker lsp
        ruff # Python linter/formatter lsp
      ];
    };

  unify.modules.general.home.programs.helix.languages.language = [
    {
      name = "python";
      auto-format = true;
      language-servers = [
        "basedpyright"
        "ruff"
      ];
    }
  ];
}
