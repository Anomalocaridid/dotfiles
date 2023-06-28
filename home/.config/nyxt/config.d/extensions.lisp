;;;; ~/.config/nyxt/config.d/extensions.lisp
;;;; helper code to load extensions and their configs

;;; load extensions and their respective config files
(defmacro load-extensions (&rest extensions)
  "Helper macro to load extensions along with config files with the same name."
  `(progn ,@(loop for extension
                  in extensions
                  collect `(define-nyxt-user-system-and-load ,(alexandria:symbolicate 'nyxt-user/ extension)
                                                             :components (,(str:concat "extension-config/" (string-downcase (symbol-name extension))))
                                                             :depends-on (,(alexandria:symbolicate 'nx- extension))))))