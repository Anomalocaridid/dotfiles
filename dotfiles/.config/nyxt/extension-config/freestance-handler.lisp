;;;; ~/.config/nyxt/extension-config/freestance-handler.lisp
;;;; settings for nx-freestance-handler extension

(define-configuration web-buffer
  ((request-resource-hook
    (reduce #'hooks:add-hook
            nx-freestance-handler:*freestance-handlers*
            :initial-value %slot-default%))))
