;;;; ~/.config/nyxt/config.d/theme.lisp
;;;; theme settings for nyxt

;;; set color palette
(defmacro define-palette (&rest colors)
  "Helper macro to set global variables for `theme' colors."
  `(progn ,@(loop for (name hex)
                  in colors
                  collect `(defparameter ,name ,hex "Color used for `theme'."))))

(define-palette (*dark-blue* "#000b1e")
                (*semi-dark-blue* "#00005f")
                (*blue* "#091833")
                (*light-blue* "#133e7c")
                (*cyan* "#0abdc6")
                (*pink* "#ea00d9")
                (*purple* "#711c91")
                (*red* "#ff0000")
                (*orange* "#f57800")
                (*white* "#d7d7d5")
                (*yellow* "#ffff00")
                (*green* "#00ff00")
                (*font* "FuraCode Nerd Font"))

(define-configuration browser
  ((theme (make-instance 'theme:theme
                         :dark-p t
                         :background-color *dark-blue*
                         :on-background-color *cyan*
                         :primary-color *pink*
                         :on-primary-color *white*
                         :secondary-color *blue*
                         :on-secondary-color *light-blue*
                         :accent-color *purple*
                         :on-accent-color *green*
                         :font-family *font*))))

(defun make-important (property)
  (str:concat property " !important"))

(define-configuration prompt-buffer
  ((style (str:concat
            %slot-value%
            (theme:themed-css (theme *browser*)  
              (*
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
               :color theme:on-primary))))))

(define-configuration web-buffer
  ((style (str:concat
            %slot-value%
            (theme:themed-css (theme *browser*)
              (*
               :font-family theme:font-family)
              ("a:hover"
               :color theme:on-accent)
              ("a:active"
               :color theme:on-secondary)
              (".button"
               :background-color (make-important theme:background) ; important necessary for gopher search buttons
               :border "1px solid"
               :border-color theme:primary
               :color theme:on-background)
              (".button:hover"
               :background-color (make-important theme:secondary) ; important necessary for gopher search buttons 
               :border "1px solid"
               :border-color (make-important theme:secondary) ; important necessary for gopher search buttons
               :color theme:on-background
               :opacity 1)
              (".button:active"
               :background-color (make-important theme:on-secondary) ; important necessary for gopher search buttons
               :border "1px solid"
               :border-color (make-important theme:on-secondary) ; important necessary for gopher search buttons
               :color theme:primary)
              (".button:visited"
               :color theme:accent)
              (".button:visited:active"
               :color theme:background)
              ;; necessary for headings on nyxt::dashboard
              ("h2, h3, h4, h5, h6"
               :color (make-important theme:primary))
              ;; "Browser" title text on nyxt:dashboard
              ("#subtitle"
               :color (make-important theme:accent)))))))

(define-configuration nyxt/repl-mode:repl-mode
  ((style (str:concat
            %slot-value%
            (theme:themed-css (theme *browser*)
              (".input"
               :background-color theme:secondary
               :border "1px solid"
               :border-color theme:primary
               :color theme:on-background)
              (".input-buffer"
               :background-color theme:background
               :color theme:on-background
               :opacity 1)
              (".input-buffer::placeholder"
               :color theme:accent))))))

(define-configuration nyxt/small-web-mode:small-web-mode
  ((style (str:concat
            %slot-value%
            (eval
              `(theme:themed-css (theme *browser*)
                ("pre"
                 :background-color theme:background)
                ("a.button.search"
                 :color theme:on-accent
                 :border-color theme:on-accent)
                ("a.button.error"
                 :color ,*red*
                 :border-color ,*red*)))))))

(define-configuration status-buffer
  ((style (str:concat
            %slot-value%
            (eval
              `(theme:themed-css (theme *browser*)
                (*
                 :font-family theme:font-family)
                ("#controls"
                 :background-color ,*semi-dark-blue*
                 :color theme:primary)
                ("#controls button:hover"
                 :color theme:on-primary)
                ("#controls button:active"
                 :color theme:on-background)
                ("#container"
                 :display "grid"
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
                 :color theme:background)))))))

(define-configuration window
  ((message-buffer-style (str:concat
                           %slot-value%
                           (theme:themed-css (theme *browser*)
                             (body
                              :font-family theme:font-family))))))

;; TODO: tweak hint mode style
;; TODO: style dark mode
