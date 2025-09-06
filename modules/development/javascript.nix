{ inputs, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      devshells.javascript.packages = with pkgs; [
        nodejs
        nodePackages.typescript-language-server
      ];
    };

  unify.modules.general.home =
    { pkgs, ... }:
    {
      home.file = {
        # Yarn config
        ".yarnrc.yml".source =
          (inputs.nixago.lib.${pkgs.system}.make {
            # Disable telemetry
            data.enableTelemetry = 0;
            output = ".yarnrc.yml";
            format = "yaml";
          }).configFile;
      };

      programs.helix.languages = {
        language-server.typescript-language-server.config.format.semicolons = "insert";
        language = [
          {
            name = "javascript";
            auto-format = true;
          }
          {
            name = "typescript";
            auto-format = true;
          }
        ];
      };
    };
}
