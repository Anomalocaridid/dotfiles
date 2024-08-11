{ lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    bats # Needed for exercism tests
  ];

  programs.helix = {
    extraPackages = with pkgs; [
      nodePackages.bash-language-server
      shellcheck # More diagnostics for language server
    ];
    languages.language = [
      {
        name = "bash";
        auto-format = true;
        indent = {
          tab-width = 4;
          unit = "    ";
        };
        formatter.command = lib.getExe pkgs.shfmt;
      }
    ];
  };
}
