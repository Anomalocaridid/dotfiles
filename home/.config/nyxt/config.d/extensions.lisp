;;;; config.d/extensions.lisp
;;;; extension config
(in-package #:nyxt-user)

(defmacro load-extensions (&rest extensions)
  "Helper macro to load extensions along with config files with the same name."
  `(progn ,@(loop for extension
                  in extensions
                  collect `(nyxt:define-nyxt-user-system-and-load
                            ,(alexandria:symbolicate 'nyxt-user/ extension)
                            :depends-on (,(alexandria:symbolicate 'nx- extension))
                            :components (,(str:concat "config.d/extensions.d/"
                                            (string-downcase (symbol-name extension))))
                                                            ))))

(nyxt:define-nyxt-user-system-and-load nyxt-user/nx-fruit
  :depends-on ("nx-fruit"))

(load-extensions search-engines
                 router)
