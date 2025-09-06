{
  perSystem =
    { pkgs, ... }:
    {
      devshells.pharo.packages = with pkgs; [ pharo ];
    };

  unify.modules.general.home =
    { config, ... }:
    {
      xdg.configFile."pharo/startup.st".text = ''
        StartupPreferencesLoader default executeAtomicItems: {
          (StartupAction
            name: 'Show any startup action errors'
            code: [
                    StartupPreferencesLoader default errors
                      ifNotEmpty: [ :errors | errors inspect ] ]).

          (StartupAction
            name: 'Close welcome window'
            code: [
                    World submorphs
                      select: [ :sm | sm isSystemWindow and: [ sm label endsWith: 'Welcome' ] ]
                      thenDo: [ :window | window delete ] ]).

          (StartupAction
            name: 'Set theme'
            code: [ PharoDarkTheme beCurrent ]).
          
          "TODO: Set text editing font"
          "(StartupAction
            name: 'Load font'
            code: [
                    FreeTypeFontProvider current updateEmbeddedFreeTypeFonts.
                    FreeTypeFontProvider current updateFromSystem.

                    StandardFonts setAllStandardFontsTo: (LogicalFont
                      familyName: '${builtins.head config.fonts.fontconfig.defaultFonts.monospace}'
                      pointSize: 12) ])."

          (StartupAction
            name: 'Install TilingWindowManager'
            code: [
                    Metacello new
                      githubUser: 'pdebruic' 
                      project: 'TilingWindowManager' 
                      commitish: 'master' 
                      path: 'packages';
                      baseline: 'TilingWindowManager';
                      onWarningLog;
                      load ]
            runOnce: true).

          (StartupAction
            name: 'Install Exercism'
            code: [
                    Iceberg remoteTypeSelector: #httpsUrl.
                    Metacello new
                      baseline: 'Exercism';
                      repository: 'github://exercism/pharo-smalltalk:main/releases/latest';
                      load ]
            runOnce: true).
        }
      '';
    };
}
