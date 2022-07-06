;;;; ~/.config/nyxt/extension-config/kaomoji.lisp
;;;; settings for the nx-kaomoji extension 

(define-configuration buffer
  ((override-map (let ((map (keymaps:make-keymap "override-map")))
                              (keymaps:define-key map
                                "C-c K" 'nx-kaomoji:kaomoji-fill)
                   map))))
