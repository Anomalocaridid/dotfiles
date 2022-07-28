;;;; ~/.config/nyxt/config.d/palette.lisp
;;;; color and font settings for nyxt

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
