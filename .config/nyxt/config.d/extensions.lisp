;;;; ~/.config/nyxt/config.d/extensions.lisp
;;;; helper code to load extensions and their configs

;;; load extensions and their respective config files
(defmacro load-extensions (&rest extensions)
  "Helper macro to load extensions along with config files with the same name."
  `(progn ,@(loop for extension
                  in extensions
                  collect `(define-nyxt-user-system-and-load ,(intern (format nil "NYXT-USER/~a" extension))
                                                             :components (,(string-downcase (format nil "extension-config/~a" extension)))
                                                             :depends-on (,(intern (format nil "NX-~a" extension)))))))