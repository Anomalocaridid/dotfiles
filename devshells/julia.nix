{ pkgs, ... }:
{
  packages = with pkgs; [
    (julia.withPackages [
      "Crayons" # Needed for OhMyREPL color scheme
      "LanguageServer"
      "OhMyREPL"
    ])
  ];
}
