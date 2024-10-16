{ pkgs, ... }:
{
  packages = with pkgs; [
    bats # Needed for exercism tests
    nodePackages.bash-language-server
    shellcheck # More diagnostics for language server
    shfmt # Formatter
  ];
}
