{
  unify.modules.libreoffice.home =
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
}
