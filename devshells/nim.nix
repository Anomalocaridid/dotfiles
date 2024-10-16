{ pkgs, ... }:
{
  packages = with pkgs; [
    nim
    nimlsp
  ];
}
