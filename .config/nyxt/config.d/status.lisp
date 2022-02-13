;; ~/.config/nyxt/config.d/status.lisp
;; status bar settings

(define-configuration status-buffer
  ((glyph-mode-presentation-p t)))

(defmacro define-glyphs (&rest glyphs)
  `(progn ,@(loop for (mode glyph)
                  in glyphs 
                  collect `(define-configuration ,mode
                             ((glyph ,glyph))))))

(define-glyphs (nyxt/blocker-mode:blocker-mode "")
               (nyxt/bookmark-mode:bookmark-mode "")
               (nyxt/certificate-exception-mode:certificate-exception-mode "")
               (nyxt/help-mode:help-mode "")
               (nyxt/message-mode:message-mode "")
               (nyxt/repl-mode:repl-mode "")
               (nyxt/small-web-mode:small-web-mode "ﰍ")
               (nyxt/vi-mode:vi-insert-mode "")
               (nyxt/vi-mode:vi-normal-mode "")
               ;; uncomment when nerd fonts fix code point issue
               ;;(nyxt/web-mode:web-mode "爵")
               ;; delete when nerd font issue is fixed
               (nyxt/web-mode:web-mode ""))
