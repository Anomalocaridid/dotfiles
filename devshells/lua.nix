{ pkgs, ... }:
{
  packages = with pkgs; [
    (lua5_4.withPackages (
      ps: with ps; [
        busted # Needed for exercism tests
      ]
    ))
    lua-language-server
  ];
}
