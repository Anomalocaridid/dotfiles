{ pkgs, ... }:
{
  home.packages = with pkgs; [
    hunspell # Required for spellcheck
    hunspellDicts.en_US # American English spellcheck dictionary
    languagetool # spelling, style. and grammer checker
    libreoffice-fresh
  ];
}
