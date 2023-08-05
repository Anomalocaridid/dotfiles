;;;; extension-config/search-engines.lisp
;;;; settings for the nx-search-engines extension
(in-package #:nyxt-user)

; TODO: Investigate more elegant solutions
(defun color-kludge (color)
  "Quick and dirty helper function to work around issues with color hex codes starting with '#'"
  (subseq color 1))

(define-configuration (buffer)
  ((search-engines
    (append %slot-default%
            (list
              (engines:duckduckgo :shortcut "ddg"
                                  :advertisements nil                                  
                                  :install-duckduckgo nil
                                  :install-reminders nil
                                  :privacy-newsletter nil
                                  :newsletter-reminders nil
                                  :help-improve-duckduckgo nil
                                  :homepage-privacy-tips nil
                                  :theme :dark
                                  :font *font*
                                  :background-color (color-kludge *dark-blue*)
                                  :header-color (color-kludge *blue*)
                                  :result-title-color (color-kludge *cyan*)
                                  :result-visited-title-color (color-kludge *pink*)
                                  :result-description-color (color-kludge *cyan*)
                                  :result-url-color (color-kludge *pink*)
                                  :result-full-urls t
                                  :result-urls-above-snippet t
                                  :result-module-color (color-kludge *blue*)
                                  :result-visible-checkmark t))))))
