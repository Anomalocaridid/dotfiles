{
  perSystem =
    { pkgs, ... }:
    {
      devshells.ballerina.packages = with pkgs; [
        ballerina
      ];
    };
}
