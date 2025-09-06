{
  perSystem =
    { pkgs, ... }:
    {
      devshells.go.packages = with pkgs; [
        delve # Debugger
        go
        golangci-lint
        golangci-lint-langserver # Language Server with linting
        gopls # Language Server
      ];
    };

  unify.modules.general.home =
    { lib, pkgs, ... }:
    {
      programs.helix.languages.language = [
        {
          name = "go";
          auto-format = true;
          formatter.command = lib.getExe' pkgs.gotools "goimports";
        }
      ];
    };
}
