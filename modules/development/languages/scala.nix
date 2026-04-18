{ inputs, ... }:
{
  # Provides shell hook for generating config files
  flake-file.inputs.nixago = {
    url = "github:nix-community/nixago";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  perSystem =
    { pkgs, ... }:
    {
      devshells.scala = {
        packages = with pkgs; [
          metals # Language server
          sbt # Build tool, required for exercism tests
          scala
          scalafmt # Code formatter
        ];

        # Scalafmt config (no global location is available)
        devshell.startup.nixago.text =
          (inputs.nixago.lib.${pkgs.stdenv.hostPlatform.system}.make {
            data.globalSection = {
              version = pkgs.scalafmt.version;
              "runner.dialect" = "scala3";
            };
            output = ".scalafmt.conf";
            format = "iniWithGlobalSection";
          }).shellHook;
      };
    };

  unify.modules.general.home.programs.helix.languages.language = [
    {
      name = "scala";
      auto-format = true;
    }
  ];
}
