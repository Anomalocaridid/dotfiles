{ pkgs, ... }:
{
  packages = with pkgs; [
    racket
  ];
  # Install language server
  # If already installed, exits quickly with a brief message, so do not worry about checking
  devshell.startup.racket-langserver.text = ''
    raco pkg install --auto racket-langserver
  '';
}
