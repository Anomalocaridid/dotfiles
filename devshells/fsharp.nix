{ pkgs, ... }:
{
  packages = with pkgs; [
    dotnet-sdk_7 # Default version currently too low for Exercism
    fantomas # F# code formatter
    fsautocomplete # F# language server
  ];
}
