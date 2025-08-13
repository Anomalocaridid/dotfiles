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
  unify.modules.development.home =
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
    };
}
