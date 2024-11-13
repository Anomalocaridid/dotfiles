{ pkgs, ... }:
{
  packages = with pkgs; [
    dotnet-sdk_8 # Default version currently too low for Exercism
    fantomas # F# code formatter
    fsautocomplete # F# language server
  ];

  # Disable telemetry
  env = [
    {
      name = "DOTNET_CLI_TELEMETRY_OPTOUT";
      value = 1;
    }
  ];
}
