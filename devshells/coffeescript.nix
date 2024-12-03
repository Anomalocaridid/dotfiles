{ pkgs, ... }:
{
  packages = with pkgs; [
    coffeescript
    nodejs
  ];
}
