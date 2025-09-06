{
  perSystem =
    { pkgs, ... }:
    {
      devshells.nasm.packages = with pkgs; [ nasm ];
    };
}
