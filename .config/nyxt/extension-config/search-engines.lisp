;; ~/.config/nyxt/extension-config/search-engines.lisp
;; settings for the nx-search-engines extension

(in-package #:nyxt-user)

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
                                  :background-color *dark-blue*
                                  :header-color *blue*
                                  :result-title-color *cyan*
                                  :result-visited-title-color *pink*
                                  :result-description-color *cyan*
                                  :result-url-color *pink*
                                  :result-full-urls t
                                  :result-urls-above-snippet t
                                  :result-module-color *blue*
                                  :result-visible-checkmark t))))))