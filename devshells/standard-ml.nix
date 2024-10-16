{ pkgs, ... }:
{
  packages = with pkgs; [
    polyml # Standard ML compiler
  ];
}
