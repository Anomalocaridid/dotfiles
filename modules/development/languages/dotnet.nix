{
  perSystem =
    { pkgs, ... }:
    {
      devshells =
        let
          # Make sure telemetry is disabled. Thanks Microsoft :/
          env = [
            {
              name = "DOTNET_CLI_TELEMETRY_OPTOUT";
              value = 1;
            }
          ];
        in
        {
          csharp = {
            inherit env;
            packages = with pkgs; [
              dotnet-sdk
              omnisharp-roslyn # C# language server
            ];
          };

          fsharp = {
            inherit env;
            packages = with pkgs; [
              dotnet-sdk
              fantomas # F# code formatter
              fsautocomplete # F# language server
            ];
          };

          visual-basic = {
            inherit env;
            packages = with pkgs; [ dotnet-sdk ];
          };
        };
    };

  unify.modules.general.home.programs.helix.languages.language = [
    {
      name = "c-sharp";
      auto-format = true;
    }
  ];
}
