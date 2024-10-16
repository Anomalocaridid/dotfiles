{ pkgs, ... }:
{
  packages = with pkgs; [
    (sbcl.withPackages (ps: with ps; [ linedit ]))
    gcc # Required to compile linedit
  ];
}
