{ pkgs, ... }:
{
  home.packages = with pkgs; [ nasm ];
}
