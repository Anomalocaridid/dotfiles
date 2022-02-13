;; ~/.config/nyxt/config.d/handlers.lisp
;; url handler configurations

;; redirect wikipedia to wikiwand
(defparameter *wikipedia-regex* "(\\w{2})?\.?wikipedia.org(/wiki/)?(.*)?$")

(defun wikiwand-handler (url)
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

;; redirect about:blank to new buffer page
(defun about-blank-handler (url)
  ;; url argument's merely to comply with expected type
  (declare (ignore url))
  (default-new-buffer-url *browser*))

;; open youtube links in freetube
(defun freetube-handler (url)
  (uiop:launch-program
    `("freetube" ,(quri:render-uri url)))
  nil)

(define-configuration buffer
  ((request-resource-hook 
    (reduce #'hooks:add-hook
      (list (url-dispatching-handler
              ;; redirect wikipedia to wikiwand
              'wikiwand-handler
              (match-domain "wikipedia.org")
              #'wikiwand-handler)
            (url-dispatching-handler
              ;; redirect about:blank to new buffer page
              'about-blank-handler
              (match-url "about:blank")
              #'about-blank-handler)
            (url-dispatching-handler
              ;; open youtube urls in freetube
              'freetube-handler
              (match-domain "youtube.com"
                            "youtu.be")
              #'freetube-handler))
      :initial-value %slot-default%))))