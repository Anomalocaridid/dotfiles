;;;; ~/.config/nyxt/config.d/hooks.lisp
;;;; hooks settings

;;; convert wikiwand link to wikipedia when copied
(defparameter *wikiwand-regex* "wikiwand\.com/(.{2})?/?((?<=/).*)$"
  "Regular expression used by `wikiwand-copy-url-handler' to identify Wikiwand URLs.")
(defparameter *wikipedia-format* "https://~a.wikipedia.org/~a~a"
  "Format string used by `wikiwand-copy-url-handler' to format Wikipedia URLs.")

(defun wikiwand-copy-url-handler ()
  "Handler to convert copied Wikiwand URLs into Wikipedia URLs.
Intended for use with `copy-url-after-hook'."
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

;;; add hook to copy-url-after-hook
(hooks:add-hook copy-url-after-hook 'wikiwand-copy-url-handler)
