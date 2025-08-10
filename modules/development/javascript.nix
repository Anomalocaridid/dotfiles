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
  flake.modules.homeManager.development =
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
