{ pkgs, ... }:
{
  packages = with pkgs; [
    delve # Debugger
    go
    golangci-lint
    golangci-lint-langserver # Language Server with linting
    gopls # Language Server
  ];
}
