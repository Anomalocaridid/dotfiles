{ pkgs, ... }:
{
  home.packages = with pkgs; [ mars-mips ];
}
