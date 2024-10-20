{ pkgs, ... }:
{
  packages = with pkgs; [
    metals # Language server
    sbt # Build tool, required for exercism tests
    scala
    # scalafmt # Code formatter
  ];
}
