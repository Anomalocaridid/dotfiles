{
  perSystem =
    { pkgs, ... }:
    {
      devshells.gleam.packages = with pkgs; [
        gleam
        erlang # Required by gleam
      ];
    };
}
