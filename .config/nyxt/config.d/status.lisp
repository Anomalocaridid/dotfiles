;;;; ~/.config/nyxt/config.d/status.lisp
;;;; status bar settings

;;; enable glyphs in status buffer
(define-configuration status-buffer
  ((glyph-mode-presentation-p t)))

;;; make all modes visible
(define-configuration mode
  ((visible-in-status-p t)))

;(defmacro define-glyphs (&rest glyphs)
;  "Helper macro to set `glyph' slot for multiple modes at once."
;  `(progn ,@(loop for (mode glyph)
;                  in glyphs 
;                  collect `(define-configuration ,mode
;                             ((glyph ,glyph))))))

(defmacro define-glyphs (&rest glyphs)
  "Helper macro to set `glyph' slot for multiple modes at once."
  `(progn ,@(loop for (mode glyph raw-p)
                  in glyphs 
                  collect `(define-configuration ,(if raw-p
                                                      mode
                                                      (read-from-string (format nil "NYXT/~a:~a" mode mode)))
                             ((glyph ,glyph))))))

;;; define glyphs for modes
(define-glyphs (autofill-mode               "")
               (annotate-mode               "")
               (base-mode                   "" t)
               ; use when nerd font code point conflicts have been resolved
               ;(base-mode                  "爵" t)
               (blocker-mode                "")
               (bookmark-mode               "")
               (certificate-exception-mode  "")
               (document-mode               "")
               (help-mode                   "")
               (hint-mode                   "")
               (history-mode                "")
               (message-mode                "")
               (password-mode               "ﳳ")
               (reduce-tracking-mode        "")
               (repl-mode                   "")
               (search-buffer-mode          "")
               (small-web-mode              "ﰍ")
               (spell-check-mode            "")
               ; use when nerd font code point conflicts have been resolved
               ;(spell-check-mode           "暈")
               (nyxt/vi-mode:vi-insert-mode "" t)
               (nyxt/vi-mode:vi-normal-mode "" t))
