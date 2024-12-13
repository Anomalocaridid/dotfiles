{ pkgs, ... }:
{
  packages = with pkgs; [
    gleam
    erlang_27 # Required by gleam, default version not recent enough for exercism tests
  ];
}
