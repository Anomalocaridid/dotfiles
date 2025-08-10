{
  perSystem =
    { pkgs, ... }:
    {
      devshells.elixir.packages = with pkgs; [
        elixir
        elixir-ls
      ];
    };
}
