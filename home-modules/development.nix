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
        palette =
          (lib.importJSON "${config.catppuccin.sources.palette}/palette.json")
          .${config.catppuccin.flavor}.colors;
      in
      # julia
      ''
        using Crayons
        import OhMyREPL: Passes.SyntaxHighlighter, colorscheme!

        function _create_catppuccin_colorscheme()
            scheme = SyntaxHighlighter.ColorScheme()
            SyntaxHighlighter.symbol!(scheme, crayon"${palette.red.hex}")
            SyntaxHighlighter.comment!(scheme, crayon"${palette.overlay2.hex}")
            SyntaxHighlighter.string!(scheme, crayon"${palette.green.hex}")
            SyntaxHighlighter.call!(scheme, crayon"${palette.blue.hex}")
            SyntaxHighlighter.op!(scheme, crayon"${palette.sky.hex}")
            SyntaxHighlighter.keyword!(scheme, crayon"${palette.mauve.hex}")
            SyntaxHighlighter.macro!(scheme, crayon"${palette.blue.hex}")
            SyntaxHighlighter.function_def!(scheme, crayon"${palette.blue.hex}")
            SyntaxHighlighter.text!(scheme, crayon"${palette.text.hex}")
            SyntaxHighlighter.error!(scheme, crayon"${palette.red.hex}")
            SyntaxHighlighter.argdef!(scheme, crayon"${palette.yellow.hex}")
            SyntaxHighlighter.number!(scheme, crayon"${palette.peach.hex}")
            scheme
        end

        SyntaxHighlighter.add!("Catppuccin", _create_catppuccin_colorscheme())
        colorscheme!("Catppuccin")
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
}
