{ pkgs, ... }:
{
  packages = with pkgs; [
    # Exercism requires a different version than C# or F# for some reason
    dotnet-sdk_7
  ];

  # Disable telemetry
  env = [
    {
      name = "DOTNET_CLI_TELEMETRY_OPTOUT";
      value = 1;
    }
  ];
}
