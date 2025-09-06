{
  perSystem =
    { pkgs, ... }:
    {
      devshells.coffeescript.packages = with pkgs; [
        coffeescript
        nodejs
      ];
    };
}
