;;;; ~/.config/nyxt/extension-config/kaomoji.lisp
;;;; settings for the nx-kaomoji extension 

(define-configuration buffer
  ((override-map (let ((map (keymap:make-keymap "override-map")))
                              (keymap:define-key map
                                "C-c K" 'nx-kaomoji:kaomoji-fill)
                   map))))