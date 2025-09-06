{
  perSystem =
    { pkgs, ... }:
    {
      devshells.v.packages = with pkgs; [ vlang ];
    };
}
