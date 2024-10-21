{ pkgs, inputs, ... }:
{
  home.file = {
    # C/C++ formatter config
    ".clang-format".source =
      (inputs.nixago.lib.${pkgs.system}.make {
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
  };
}
