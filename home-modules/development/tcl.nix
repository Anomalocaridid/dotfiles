{ pkgs, ... }:
{
  home.packages = with pkgs; [
    tcl
    eltclsh # Better REPL
  ];
}
