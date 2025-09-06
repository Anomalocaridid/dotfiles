{
  perSystem =
    { pkgs, ... }:
    {
      devshells.mips.packages = with pkgs; [
        mars-mips
      ];
    };
}
