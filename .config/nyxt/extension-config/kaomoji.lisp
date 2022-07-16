;;;; ~/.config/nyxt/extension-config/kaomoji.lisp
;;;; settings for the nx-kaomoji extension 

(define-configuration buffer
  ((override-map (let ((map (make-keymap "override-map")))
                              (define-key map
                                "C-c K" 'nx-kaomoji:kaomoji-fill)
                   map))))
