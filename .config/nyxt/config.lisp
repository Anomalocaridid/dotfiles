;;;; ~/.config/nyxt/config.lisp
;;;; main config file for Nyxt

;;; set terminal command
(defparameter *terminal* '("wezterm" "start" "--")
  "Preferred command to run terminal programs with")

;;; misc settings
(define-configuration browser
   ;; never restore session
  ((session-restore-prompt :never-restore)
   ;; set external editor
   (external-editor-program (append *terminal* '("hx")))))

;;; enable default modes
(define-configuration buffer
  ((default-modes (append '(nyxt/blocker-mode:blocker-mode
                            nyxt/reduce-tracking-mode:reduce-tracking-mode)
                          %slot-default%))))

;;; load all lisp files in ./config.d
(dolist (file (directory
                (nyxt-config-file
                  (pathname "config.d/*.lisp"))))
        (load file))

;;; load extensions
(asdf:load-system :nx-fruit)
(load-extensions freestance-handler
                 kaomoji
                 search-engines
                 tailor)