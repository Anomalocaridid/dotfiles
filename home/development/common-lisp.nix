{ config, pkgs, ... }: {
  home = {
    packages = with pkgs; [
      sbcl
      gcc # Required to compile linedit
    ];
    file.".sbclrc".text = #common-lisp
      ''
        ;;; The following lines added by ql:add-to-init-file:
        #-quicklisp
        (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                               (user-homedir-pathname))))
          (when (probe-file quicklisp-init)
            (load quicklisp-init)))

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

  systemd.user.services.quicklisp-setup = {
    Unit = {
      Description = "Ensure Quicklisp is set up for sbcl";
      ConditionPathExists = "!${config.home.homeDirectory}/quicklisp/setup.lisp";
    };
    Service = {
      Type = "oneshot";
      ExecStartPre = "${pkgs.curl}/bin/curl https://beta.quicklisp.org/quicklisp.lisp --output /tmp/quicklisp.lisp";
      ExecStart = ''
        ${pkgs.sbcl}/bin/sbcl --no-userinit --non-interactive --load /tmp/quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:quickload "linedit")'
      '';
      # Sometimes fails on first try, but succeeds on second
      # Possibly some issue with downloading stuff
      Restart = "on-failure";
    };
    Install.WantedBy = [ "default.target" ];
  };
}
