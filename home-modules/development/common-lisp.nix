{ ... }:
{
  home.file.".sbclrc".text = # scheme
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
