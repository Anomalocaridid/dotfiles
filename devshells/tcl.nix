{ pkgs, ... }:
{
  packages = with pkgs; [
    tcl
    eltclsh # Better tcl REPL
  ];
}
