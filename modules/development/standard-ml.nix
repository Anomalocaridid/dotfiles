{
  perSystem =
    { pkgs, ... }:
    {
      devshells.standard-ml.packages = with pkgs; [
        polyml # Standard ML compiler
      ];
    };
}
