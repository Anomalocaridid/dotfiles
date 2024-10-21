{
  pkgs,
  inputs,
  ...
}:
{
  packages = with pkgs; [
    metals # Language server
    sbt # Build tool, required for exercism tests
    scala
    # scalafmt # Code formatter
  ];

  # Scalafmt config (no global location is available)
  devshell.startup.nixago.text =
    (inputs.nixago.lib.${pkgs.system}.make {
      data.globalSection = {
        version = pkgs.scalafmt.version;
        "runner.dialect" = "scala3";
      };
      output = ".scalafmt.conf";
      format = "iniWithGlobalSection";
    }).shellHook;
}
