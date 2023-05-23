;;;; ~/.config/nyxt/config.d/handlers.lisp
;;;; url handler configurations

;;; redirect wikipedia to wikiwand
(defparameter *wikipedia-regex* "(\\w{2})?\.?wikipedia.org(/wiki/)?(.*)?$"
  "Regular expression used by `wikiwand-handler' to identify Wikipedia URLs.")

(defun wikiwand-handler (url)
  "Handler to redirect Wikipedia pages to Wikiwand pages.
Intended for use with `request-resource-hook'."
  (let ((url-text (str:concat (quri:uri-host url)
                              (quri:uri-path url)))
        (params (quri:uri-query-params url)))
    (if (or (assoc "oldformat" params :test #'string=)
            (assoc "search" params :test #'string=))
        url
        (cl-ppcre:register-groups-bind (lang nil article)
            (*wikipedia-regex* url-text)
        (quri:make-uri :scheme "https"
                       :host "www.wikiwand.com"
                       :path (cond ((string-equal article "Main_Page") "news")
                                   ((string-equal article "/") nil)
                                   (t (str:concat (if (or (string-equal lang "ww")
                                                          (not lang))
                                                      "en"
                                                      lang)
                                                  "/"
                                                  article))))))))

;;; redirect about:blank to new buffer page
(defun about-blank-handler (url)
  "Handler to redirect about:blank to `default-new-buffer-url' in `*browser*'.
The given url will be ignored and is only used to comply with the type expected by `request-resource-hook'.
Intended for use with `request-resource-hook'."
  ;; url argument's merely to comply with expected type
  (declare (ignore url))
  (default-new-buffer-url *browser*))

;;; open youtube links in freetube
(defun freetube-handler (url)
  "Handler to open YouTube URLs in FreeTube.
Intended for use with `request-resource-hook'."
  (uiop:launch-program
    `("freetube" ,(quri:render-uri url)))
  nil)

(defmacro define-url-handlers (&rest handlers)
  "Helper macro to set `request-resource-hook' handlers in `buffer'."
  `(define-configuration buffer
     ((request-resource-hook
       (reduce #'hooks:add-hook
         (list ,@(loop for (name pred)
                       in handlers
                       collect `(url-dispatching-handler ,name
                                                         ,pred
                                                         (symbol-function ,name))))
               :initial-value %slot-default%)))))

;;; set url handlers
(define-url-handlers ('about-blank-handler (match-url "about:blank"))
                     ;('freetube-handler (match-domain "youtube.com"
                     ;                                 "youtu.be"))
                     ('wikiwand-handler (match-domain "wikipedia.org")))
