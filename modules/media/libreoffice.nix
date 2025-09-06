{ config, ... }:
let
  inherit (config.flake.meta) persistDir username;
in
{
  unify.modules.general = {
    nixos.environment.persistence.${persistDir}.users.${username}.directories = [
      ".config/LanguageTool" # LanguageTool settings
      ".config/libreoffice" # Libreoffice settings
    ];

    home =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          hunspell # Required for spellcheck
          hunspellDicts.en_US # American English spellcheck dictionary
          languagetool # Spelling, style, and grammer checker
          libreoffice-fresh
        ];

        xdg.mimeApps.defaultApplications =
          let
            officeSuite = "startcenter.desktop";
          in
          {
            "application/msword" = "writer.desktop";
            "application/vnd.ms-excel" = "calc.desktop";
            "application/vnd.ms-powerpoint" = "impress.desktop";
            "application/vnd.oasis.opendocument.*" = officeSuite;
            "application/vnd.openxmlformats-officedocument.*" = officeSuite;
          };
      };
  };
}
