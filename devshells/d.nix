{ pkgs, ... }:
{
  packages = with pkgs; [
    dmd # D compiler
    dub # D build tool
    serve-d # D language server
  ];
}
