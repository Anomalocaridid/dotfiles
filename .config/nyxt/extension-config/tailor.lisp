;;;; ~/.config/nyxt/extension-config/tailor.lisp
;;;; settings for nx-tailor extension

(define-configuration web-buffer
  ((default-modes (append '(tailor:tailor-mode) %slot-default%))))

(define-glyphs (nx-tailor:tailor-mode "" t))

;;; define custom theme
(define-configuration tailor:tailor-mode
  ((tailor:themes
    (list
      (tailor:make-theme "Cyberpunk Neon"
                         :dark-p t
                         :background-color *dark-blue*
                         :on-background-color *cyan*
                         :primary-color *pink*
                         :on-primary-color *white*
                         :secondary-color *blue*
                         :on-secondary-color *light-blue*
                         :accent-color *purple*
                         :on-accent-color *green*
                         :font-family *font*
                         :cut (make-instance 'tailor:cut))))))

;;; define custom css
(define-configuration tailor:cut
  ((tailor:name "Cyberpunk Neon")
   (tailor:prompt
     '((*
        :font-family theme:font-family)
       ("#prompt, #prompt-extra, #prompt-area"
        :background-color theme:secondary
        :color theme:on-background)
       (button
        :border-radius "4px")
       ("button[title=vi-normal-mode], button[title=vi-insert-mode]:hover"
        :background-color theme:accent
        :border-color theme:accent
        :color theme:on-background)
       ("button[title=vi-insert-mode], button[title=vi-normal-mode]:hover"
        :background-color theme:primary
        :border-color theme:primary
        :color theme:on-primary)
       (".source-name"
        :color theme:on-background
        :font-weight "bold")
       (".source-content th"
        :background-color theme:background
        :border "1px solid"
        :border-color theme:primary
        :color theme:on-background)
       ("#selection"
        :color theme:on-background
        :font-weight "bold")
       (".marked"
        :background-color theme:primary
        :color theme:on-primary)))
   (tailor:buffer
     '((*
        :font-family theme:font-family)
       ("a:hover"
        :color theme:on-accent)
       ("a:active"
        :color theme:on-secondary)
       (".button"
        :background-color theme:background
        :border "1px solid"
        :border-color theme:primary
        :color theme:on-background)
       (".button:hover"
        :background-color theme:secondary
        :border "1px solid"
        :border-color (tailor:make-important theme:secondary)
        :color theme:on-background
        :opacity 1)
       (".button:active"
        :background-color theme:on-secondary
        :border "1px solid"
        :border-color (tailor:make-important theme:on-secondary)
        :color theme:primary)
       (".button:visited"
        :color theme:accent)
       (".button:visited:active"
        :color theme:background)
       ("h2, h3, h4, h5, h6"
        :color (tailor:make-important theme:primary))
       ;; "Browser" title text on nyxt:dashboard
       ("#subtitle"
        :color (tailor:make-important theme:accent))
       ;; repl styles
       (".input"
        :background-color (tailor:make-important theme:secondary)
        :border "1px solid"
        :border-color theme:primary
        :color theme:on-background)
       (".input-buffer"
        :background-color theme:background
        :color (tailor:make-important theme:on-background)
        :opacity 1)
       (".input-buffer::placeholder"
        :color theme:accent)
       ;; Gopher/Gemini styles
       ("pre"
        :background-color (tailor:make-important theme:background))
       ("a.button.search"
        :color theme:on-accent
        :border-color theme:accent)))
       ;("a.button.error")
   (tailor:status
     `((*
        :font-family theme:font-family)
       ("#controls"
        :background-color ,*semi-dark-blue*
        :color theme:primary)
       ("#controls button:hover"
        :color theme:on-primary)
       ("#controls button:active"
        :color theme:on-background)
       ("#container"
        ; original style before truncation
        ;:grid-template-columns "90px minmax(auto, 30ch) 1fr minmax(220px)")
        :grid-template-columns "90px minmax(auto, 30ch) 1fr auto")
       ("#url"
        :background-color theme:on-secondary
        :color theme:on-background)
       (".button:hover"
        :opacity 1)
       ("#url .button:hover"
        :color theme:primary)
       ("#url .button:active"
        :color theme:on-primary)
       ("#tabs"
        :background-color theme:background
        :color theme:on-background)
       ("#tabs .button:hover"
        :color theme:primary)
       ("#tabs .button:active"
        :color theme:on-primary)
       ("#modes .button:hover"
        :color theme:on-background)
       ("#modes .button:active"
        :color theme:background)))
   (tailor:message
     '((body
        :font-family theme:font-family)))))
