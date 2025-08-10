{
  perSystem =
    { pkgs, ... }:
    {
      devshells.scheme.packages = with pkgs; [
        guile
        gnumake # Needed for exercism tests
      ];
    };
}
