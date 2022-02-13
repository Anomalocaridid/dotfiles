;; ~/.config/nyxt/config.d/hooks.lisp
;; hooks settings

;; convert wikiwand link to wikipedia when copied
(defparameter *wikiwand-regex* "wikiwand\.com/(.{2})?/?((?<=/).*)$")
(defparameter *wikipedia-format* "https://~a.wikipedia.org/~a~a")

(defun wikiwand-copy-url-handler ()
  (let ((url (clipboard-text *browser*)))
    (if (string= (quri:uri-domain (quri:uri url))
                 "wikiwand.com")
        (cl-ppcre:register-groups-bind (lang article)
            (*wikiwand-regex* url)
          (copy-to-clipboard
            (format nil *wikipedia-format*
                    (or lang
                        "en")
                    (if (string= article "")
                        ""
                        "wiki/")
                    (if (and (string-equal article "news")
                             (not lang))
                        "Main_Page"
                        article)))))))

(hooks:add-hook copy-url-after-hook 'wikiwand-copy-url-handler)
