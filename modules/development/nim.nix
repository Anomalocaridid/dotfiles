{
  perSystem =
    { pkgs, ... }:
    {
      devshells.nim.packages = with pkgs; [
        nim
        nimlsp
      ];
    };
}
