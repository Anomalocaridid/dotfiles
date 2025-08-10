{ inputs, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      devshells.python.packages = with pkgs; [
        (python3.withPackages (
          ps: with ps; [
            pytest # Needed for exercism tests
            (inputs.ignis.packages.${pkgs.system}.ignis.override {
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
}
