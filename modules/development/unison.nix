{
  perSystem =
    { pkgs, ... }:
    {
      devshells.unison.packages = with pkgs; [ unison-ucm ];
    };
}
