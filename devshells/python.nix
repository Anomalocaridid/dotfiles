{ pkgs, ... }:
{
  packages = with pkgs; [
    (python3.withPackages (
      ps: with ps; [
        pytest # Needed for exercism tests
      ]
    ))
    basedpyright # Python typechecker lsp
    ruff # Python linter/formatter lsp
  ];
}
