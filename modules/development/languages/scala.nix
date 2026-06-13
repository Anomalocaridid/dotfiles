{
  perSystem =
    { lib, pkgs, ... }:
    {
      devshells.scala = {
        packages = with pkgs; [
          metals # Language server
          sbt # Build tool, required for exercism tests
          scala
          scalafmt # Code formatter
        ];

        # Scalafmt config (no global location is available)
        devshell.startup.".scalafmt.conf".text = ''
          cat << EOF > $PWD/.scalafmt.conf 
          ${lib.generators.toINIWithGlobalSection { } {
            globalSection = {
              version = pkgs.scalafmt.version;
              "runner.dialect" = "scala3";
            };
          }}
          EOF
        '';
      };
    };

  unify.modules.general.home.programs.helix.languages.language = [
    {
      name = "scala";
      auto-format = true;
    }
  ];
}
