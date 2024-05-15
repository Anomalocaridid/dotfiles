{ lib, pkgs, ... }:
{
  home.packages = with pkgs; [ polyml ];

  programs.helix.languages.language = [
    {
      name = "sml";
      auto-format = true;
      formatter.command = lib.getExe pkgs.smlfmt;
      indent = {
        tab-width = 2;
        unit = "  ";
      };
    }
  ];
}
