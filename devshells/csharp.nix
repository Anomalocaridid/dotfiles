{ pkgs, ... }:
{
  packages = with pkgs; [
    dotnet-sdk_7 # Default version currently too low for Exercism
    omnisharp-roslyn # C# language server
  ];
}
