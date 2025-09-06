{
  perSystem =
    { pkgs, ... }:
    {
      devshells.tcl.packages = with pkgs; [
        tcl
        eltclsh # Better tcl REPL
      ];
    };
}
