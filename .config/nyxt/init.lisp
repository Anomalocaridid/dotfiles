;;;; ~/.config/nyxt/init.lisp
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

;;; enable blocker mode
(define-configuration web-buffer
  ((default-modes (append '(nyxt/blocker-mode:blocker-mode) %slot-default%))))

;;; load all lisp files in ./config.d
(dolist (file (directory
                (nyxt-init-file
                  (pathname "config.d/*.lisp"))))
        (load file))

;;; load extensions and their respective config files
(defmacro load-extension (extension)
  "Helper macro to load extensions along with config files with the same name."
  `(load-after-system ',(intern (format nil "NX-~a" extension))
                      (nyxt-init-file ,(string-downcase (format nil "extension-config/~a.lisp" extension)))))

;;; load extensions
(load-extension search-engines)
(load-extension kaomoji)
(load-extension freestance-handler)
(asdf:load-system :nx-fruit)