;;;; extension-config/router.lisp
;;;; settings for the nx-router extension
(in-package #:nyxt-user)

(define-configuration web-buffer
  ((default-modes (pushnew 'nx-router:router-mode %slot-value%))))

(define-configuration router:router-mode
  ((router:routers
    (list
      (make-instance 'router:redirector
                     :name 'wikiwand
                     :route (match-domain "wikipedia.org")
                     :redirect
                      '(("www.wikiwand.com/news" . ".*Main_Page$")
                        ("www.wikiwand.com/en/\\1" . "www.wikipedia.org/wiki/(.*)")
                        ("www.wikiwand.com/\\1/\\2" . "(\\w*).wikipedia.org/wiki/(.*)")))
      (make-instance 'router:redirector
                     :name 'handlr
                     :route (match-domain "youtube.com" "youtu.be")
                     :redirect "www.youtube.com")
      (make-instance 'router:opener
                     :name 'handlr
                     :resource (lambda (url)
                                 (uiop:launch-program (list "handlr" "open" url))))
      ; Use teddit specifically
      (make-instance 'router:redirector
                     :name 'teddit
                     :route (match-domain "reddit.com")
                     :redirect '(("https://farside.link/teddit/\\1" . ".*://[^/]*/(.*)$")))
      ; Does not work with generic farside method
      (make-instance 'router:redirector
                     :name 'dumb
                     :route (match-domain "genius.com")
                     :redirect '(("https://farside.link/dumb/\\1" . ".*://[^/]*/(.*)$")))
      ; Does not work with generic farside method
      (make-instance 'router:redirector
                     :name 'lingva
                     :route (match-host "translate.google.com")
                     :redirect '(("https://farside.link/lingva/\\1" . ".*://[^/]*/(.*)$")))
      ; Redirect using farside.link
      (make-instance 'router:redirector
                     :name 'farside
                     :route (match-domain "stackoverflow.com"
                                          "reddit.com"
                                          "twitter.com"
                                          "instagram.com"
                                          "fandom.com"
                                          "imdb.com"
                                          "tiktok.com"
                                          "quora.com"
                                          "imgur.com"
                                          "medium.com")
                     :redirect '(("https://farside.link/\\1" . "https://(.*)")))))))

; TODO: Figure out how to reverse redirect for copying URLs
; TODO: Figure out how to fix cannot open in iframe errors with nyxt::on-signal-notify-uri
