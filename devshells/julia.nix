{ pkgs, ... }:
{
  packages = with pkgs; [
    (julia.withPackages [
      "LanguageServer"
    ])
  ];
}
