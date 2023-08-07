{ pkgs, ... }:
{
  home.packages = with pkgs; [
    nodejs
    nodePackages.typescript-language-server
  ];
}
