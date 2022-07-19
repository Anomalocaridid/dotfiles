;;;; ~/.config/nyxt/config.lisp
;;;; main config file for Nyxt

;;; set terminal command
(defparameter *terminal* '("wezterm" "start" "--")
  "Preferred command to run terminal programs with")

;;; misc settings
(define-configuration browser
   ;; never restore session
  ((restore-session-on-startup-p nil)
   ;; set external editor
   (external-editor-program (append *terminal* '("hx")))))

;;; enable default modes
(define-configuration buffer
  ((default-modes (append '(nyxt/blocker-mode:blocker-mode
                            nyxt/reduce-tracking-mode:reduce-tracking-mode)
                          %slot-default%))))

;;; load all lisp files in ./config.d
;;; TODO: get file names dynamically
(define-nyxt-user-system-and-load nyxt-user/config
                                  :components ("config.d/extensions"
                                               "config.d/handlers"
                                               "config.d/hooks"
                                               "config.d/palette"
                                               "config.d/status"))

;;;; load extensions
(asdf:load-system :nx-fruit)
(load-extensions freestance-handler
                 kaomoji
                 search-engines
                 tailor)
