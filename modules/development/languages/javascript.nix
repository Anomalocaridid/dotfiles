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
      # Yarn config
      # Disable telemetry
      home.file.".yarnrc.yml".source = pkgs.writers.writeYAML ".yarnrc.yml" {
        enableTelemetry = 0;
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
