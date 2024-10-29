{ pkgs, ... }:
{
  packages = with pkgs; [
    perl
    perl540Packages.PerlCritic
    perl540Packages.PerlTidy
    perlnavigator # Language server
  ];
}
