{
  perSystem =
    { pkgs, ... }:
    {
      devshells.cobol.packages = with pkgs; [
        gnucobol
      ];
    };
}
