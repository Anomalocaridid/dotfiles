{ pkgs, ... }:
{
  packages = with pkgs; [
    guile
    gnumake # Needed for exercism tests
  ];
}
