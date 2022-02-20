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
                (*green* "#00ff00"))

;;; set preferred font
(defparameter *font* "FuraCode Nerd Font"
  "Preferred font to use for `theme'.")

(defparameter *cyberpunk-neon* (make-instance 'theme:theme
                                              :dark-p t
                                              :background-color *dark-blue*
                                              :text-color *cyan*
                                              :primary-color *pink*
                                              :secondary-color *blue*
                                              :tertiary-color *purple*
                                              :quaternary-color *blue*
                                              :accent-color *green*
                                              :font-family *font*)
  "Custom dark theme.")

;;; default style for all elements (* selector)
;;; not all classes require color attribute
;;; do not quote theme variables
(defparameter *all-elements* '("*"
                               :font-family theme:font-family
                               :color theme:text)
  "Style to be applied to all elements in all classes.
Used by `define-style'.")

;;; set default border style
(defparameter *border* "1px solid"
  "Default border weight and style.
Used by `border-style'.")

(defun border-style (color)
  "Helper function to create border style with a given color using `*border*'."
  (list :border *border* :border-color `(quote ,color)))

(defun link-style (selector css hover-css active-css)
  "Helper function to style elements using :hover and :active pseudo-classes."
  `(,(cons selector css)
    ,(cons (str:concat selector ":hover") hover-css)
    ,(cons (str:concat selector ":active") active-css)))

(defun button-style (selector css button-css hover-css active-css)
  "Helper function to style button elements with :hover and :active pseudo-classes."
  (cons (cons selector css)
        (link-style (str:concat selector " button")
                    button-css
                    hover-css
                    active-css)))

;;; vi mode class styles
(defparameter *vi-mode-classes* `(,@(link-style ".vi-insert-mode"
                                                '(:background-color 'theme:primary
                                                  :color *white*)
                                                '(:color 'theme:text)
                                                '(:background-color 'theme:tertiary))
                                  ,@(link-style ".vi-normal-mode"
                                                '(:background-color 'theme:tertiary
                                                  :color 'theme:text)
                                                '(:color *white*)
                                                '(:background-color 'theme:primary)))
  "Style for vi mode indicators in `status-buffer' and `prompt'.")
                                                
;;; TODO: Change it to get rid of the need for eval
;;; configure style for a given class
(defmacro define-style (class &rest css)
  "Helper macro to set `style' slot for a given class."
       ;; window uses a different slot name
  (let ((slot (if (eql class 'window)
                  'message-buffer-style
                  'style))
        ;; setting a global color messes up vi mode on status-buffer
        (all-elements (if (eql class 'status-buffer)
                          (subseq *all-elements* 0 3)
                          *all-elements*)))
    `(define-configuration ,class
       ((,slot
         (str:concat %slot-default%
                     (theme:themed-css
                       (theme *browser*)
                       ,all-elements
                       ,@(mapcar (lambda (x) (eval `(list ,@x)))
                                 css))))))))

;;; Set browser-wide theme
(define-configuration browser
  ((theme *cyberpunk-neon*)))

;;; TODO: fix .marked and .selected
;;; style command prompt
(eval
  `(define-style prompt-buffer
                 ("#prompt, #prompt-extra, #prompt-area-vi"
                  :background-color 'theme:secondary)
                 ,@*vi-mode-classes*
                 ("#input"
                  :background-color 'theme:background)
                 (".source-name, .source-content"
                  :color 'theme:text)
                 (".source-content th"
                   :background-color 'theme:background
                   ,@(border-style 'theme:primary))
                 ("#selection"
                  :background-color 'theme:tertiary
                  :font-weight "bold")
                 (".marked"
                  :background-color 'theme:primary
                  :color *white*)
                 (".selected"
                  :background-color 'theme:primary
                  :color *white*)))

;;; style internal interface
(eval
   `(define-style buffer
                  ,@(link-style "a"
                                '()
                                '(:color 'theme:accent)
                                '(:color *light-blue*))
                  ,@(link-style ".button"
                                `(:background-color 'theme:background
                                  ,@(border-style 'theme:primary)
                                  :color 'theme:text
                                  :text-align "center")
                                `(:background-color 'theme:secondary
                                  ,@(border-style 'theme:secondary))
                                `(:background-color *light-blue*
                                  ,@(border-style 'theme:secondary)
                                  :color 'theme:primary))
                  (".button:visited"
                   :color 'theme:tertiary)
                  (".button:visited:active"
                   :color 'theme:background)))

;;; style status bar near bottom of screen
(eval
  `(define-style status-buffer
                 ("#controls"
                  :background-color *semi-dark-blue*
                  :color 'theme:primary)
                 ("#controls button:hover"
                  :color *white*)
                 ("#controls button:active"
                  :color 'theme:text)
                 ,@*vi-mode-classes*
                 ,@(button-style "#url"
                                 '(:background-color *light-blue*)
                                 '(:color 'theme:text)
                                 '(:color 'theme:primary)
                                 '(:color *white*))
                 ,@(button-style "#tabs"
                                 '(:background 'theme:background)
                                 '(:color 'theme:text)
                                 '(:color 'theme:primary)
                                 '(:color *white*))
                 ,@(button-style "#modes"
                                 '(:background-color 'theme:primary)
                                 '(:color *white*)
                                 '(:color 'theme:text)
                                 '(:color 'theme:background))))
                   
;;; style message box below status bar
(define-style window)

;;; TODO: pink border around #prompt and #input
;;; style lisp-repl
(define-style nyxt/repl-mode:repl-mode
               ("#input"
                :background-color 'theme:secondary)
               ("#input-buffer"
                :background-color 'theme:background)
               ("#prompt"
                :color 'theme:text))

;;; style tweaks for gopher/gemini pages
(define-style nyxt/small-web-mode:small-web-mode
              ("pre"
               :background-color 'theme:background)
              (".search"
               :background-color 'theme:background
               :border-color 'theme:accent
               :color 'theme:accent)
              (".search:hover, .search b"
               :color 'theme:accent)
              (".error"
               :background-color 'theme:background
               :color *red*))

;;; style tweaks for bookmark list
(eval
  `(define-style nyxt/bookmark-mode:bookmark-mode
                 ("summary"
                  :background-color 'theme:background
                  ,@(border-style 'theme:primary)
                  :color 'theme:text)))