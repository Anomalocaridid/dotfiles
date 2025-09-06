{
  perSystem =
    { pkgs, ... }:
    {
      devshells.common-lisp = {
        packages = with pkgs; [
          roswell # Common Lisp build tool/test runner
          (sbcl.withPackages (ps: with ps; [ linedit ]))
        ];

        # Install necessary dependencies like quicklisp
        # Rather noisy, so check if they need to be installed first
        devshell.startup.roswell.text = ''
          if [[ ! -d ~/.roswell ]]; then
            ros help
          fi
        '';
      };
    };

  # Common Lisp repl config
  unify.modules.general.home.home.file.".sbclrc".text = # scheme
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
}
