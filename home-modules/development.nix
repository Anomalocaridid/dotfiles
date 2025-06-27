{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  nixagoLib = inputs.nixago.lib.${pkgs.system};
in
{
  home.file = {
    # C/C++ formatter config
    ".clang-format".source =
      (nixagoLib.make {
        data = {
          BasedOnStyle = "LLVM";
          IndentWidth = 4;
          IndentCaseLabels = true;
          AlignConsecutiveDeclarations = true;
        };
        output = "clang-format";
        format = "yaml";
      }).configFile;
    ".ghci".text = ''
      :set prompt "\ESC[1;35mλ> \ESC[m"
    '';
    ".julia/config/startup.jl".text =
      let
        mkUpper =
          str:
          (lib.toUpper (builtins.substring 0 1 str)) + (builtins.substring 1 (builtins.stringLength str) str);
      in
      # julia
      ''
        import OhMyREPL
        include("${inputs.catppuccin-ohmyrepl}/catppuccin.jl")
        OhMyREPL.colorscheme!("Catppuccin${mkUpper config.catppuccin.flavor}")
      '';
    # Common Lisp repl config
    ".sbclrc".text = # scheme
      ''
        ;;; Load included packages without quicklisp 
        (load (sb-ext:posix-getenv "ASDF"))
        (asdf:load-system 'linedit)

        ;;; Check for --no-linedit command-line option.
        (if (member "--no-linedit" sb-ext:*posix-argv* :test 'equal)
            (setf sb-ext:*posix-argv*
                  (remove "--no-linedit" sb-ext:*posix-argv* :test 'equal))
            (when (interactive-stream-p *terminal-io*)
              (require :sb-aclrepl)
              (require :linedit)
              (funcall (intern "INSTALL-REPL" :linedit) :wrap-current t)))
      '';
    # Yarn config
    ".yarnrc.yml".source =
      (nixagoLib.make {
        # Disable telemetry
        data.enableTelemetry = 0;
        output = ".yarnrc.yml";
        format = "yaml";
      }).configFile;
  };

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
}
