{ pkgs, ... }:
{
  packages = with pkgs; [
    elixir
    elixir-ls
  ];
}
