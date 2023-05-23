;;;; ~/.config/nyxt/config.d/schemes.lisp
;;;; Custom internal schemes


; delete when done
;(defparameter *test-url* "https://wikiless.org/wiki/Potato?lang=en")
;(defparameter *test-doc* (plump:parse (dex:get *test-url*)))

;;; Custom Wikiwand-inspired Wikipedia frontend
(defun wiki-scheme-handler (url buffer)
  (let* ((request-url (str:replace-first "wiki:" "https://" url))
         (header (tidy-document request-url))
         (body (extract-element "#bodyContent" header))
         (contents (extract-element "#toc" body))
         (toggle-check (extract-element "#toctogglecheckbox" contents))
         (toggle (extract-element ".toctogglespan" contents)))
    (spinneret:with-html-string
      (:style (str:concat (style buffer) (wiki-style)))
        (:div :style "overflow: hidden; width: 100%; height: 100%;"
          (:div :style "float: left; overflow-y: scroll; height: 100%;" ; width: 15vw;"
            (:raw (plump:serialize toggle-check nil))
            (:raw (plump:serialize toggle nil))
            (:raw (plump:serialize contents nil)))
          (:div :style "overflow-y: scroll; height: 100%;"
            (:raw (plump:serialize header nil))
            (:hr)
            (:raw (plump:serialize body nil)))))))

(defun extract-element (selector document)
  (plump:remove-child
    (elt (clss:select selector document) 0)))

(defun tidy-document (url)
  (let* ((document (plump:parse (dex:get url)))
	     (ref-header (plump:parent (elt (clss:select "#References" document) 0)))
	     (read-header (plump:parent (elt (clss:select "#Further_reading" document) 0)))
	     (link-header (plump:parent (elt (clss:select "#External_links" document) 0)))
 )
    (progn
	  (plump:parse
	    (spinneret:with-html-string
		  (:input :type "checkbox" :id "reftogglecheckbox" :class "sectiontogglecheckbox" :checked "true")
		  (:label :class "sectiontogglelabel" :for "reftogglecheckbox"))
	    :root ref-header)
	  (plump:parse
	    (spinneret:with-html-string
		  (:input :type "checkbox" :id "readtogglecheckbox" :class "sectiontogglecheckbox" :checked "true")
		  (:label :class "sectiontogglelabel" :for "readtogglecheckbox"))
	    :root read-header)
	  (plump:parse
	    (spinneret:with-html-string
		  (:input :type "checkbox" :id "linktogglecheckbox" :class "sectiontogglecheckbox" :checked "true")
		  (:label :class "sectiontogglelabel" :for "linktogglecheckbox"))
	    :root link-header)
      (remove-elements
        (clss:select "style, .noprint, #mw-hidden-catlinks, #mw-navigation, #footer, th ul, .mw-jump-link, .mw-editsection, link" document))
      (plump:traverse document
	                  (alexandria:rcurry #'plump:remove-attribute "style")
                      :test #'plump:element-p)
      (fix-wiki-links document "a" "href" (quri:uri url)
        (lambda (element attr base-url)
          (str:replace-first
            "https://"
            "wiki:"
            (fix-wiki-link element attr base-url))))
      (fix-wiki-links document "img" "src" (quri:uri url) #'fix-wiki-link)
	)
	))

(defun wiki-style () 
  (theme:themed-css (theme *browser*)
    ("th"
     :background-color theme:background
     :border "1px solid"
     :border-color theme:primary
     :color theme:on-background)
    (".infobox td"
     :border-style "none")
    ("td"
     :border-bottom-style "none")
    ("tr ul li, tr ul ul, tr ul li li"
     :display "inline")
    ("tr ul li::after"
     ;FIXME: figure out how to get content intepreted properly
     ;:content " · "
     ;:content "\"·\""
     :font-weight "bold")
    (".infobox"
     :float "right"
     :width "0.1%")
    (".infobox th"
     :text-align "center")
    ("#toctogglecheckbox"
     :display "none")
    ("#toctogglecheckbox:checked ~ #toc .toctitle #mw-toc-heading, #toctogglecheckbox:checked ~ #toc ul"
     :display "none")
    (".toctogglelabel"
     :width 0
     :height 0  
     :border-top "20px solid transparent"
     :border-bottom "20px solid transparent"
     :border-right (str:concat "20px solid" theme:primary)
     :float "right"
     :margin-right "10px")
    (".toctogglelabel:hover"
     :border-right-color theme:on-accent)
    (".toctogglelabel:active"
     :border-right-color theme:on-secondary)
    ("#toctogglecheckbox:checked + .toctogglespan .toctogglelabel"
     :border-left (str:concat "20px solid" theme:primary)
     :border-right "0px solid transparent"
     :position "fixed")
    ("#toctogglecheckbox:checked + .toctogglespan .toctogglelabel:hover"
     :border-left-color theme:on-accent
	 :display "show")
	("#toctogglecheckbox:checked + .toctogglespan .toctogglelabel:active"
     :border-left-color theme:on-secondary)
	("div:has(#toctogglecheckbox:checked)"
	 :width 0)
    (".mw-parser-output img"
     :float "right") ; article protected lock
    ("body"
     :overflow "hidden")
    ("#toc ul"
     :margin "0px"
     ;:padding "0px"
     :padding-left "20px")
    (".toclevel-1"
     :font-size "0.95em")
    (".toclevel-2"
     :font-size "0.95em")
    (".toclevel-3"
     :font-size "0.85em")
    ("#toc ul li ul"
     :border-bottom-color theme:secondary)
;    ("p"
;     :font-size "0.95em"
;  )
    (".floatright"
     :float "right")
    (".tright"
     :float "right")
	("body"
	 :margin "5px")
	("#mw-toc-heading"
	 :margin-top "30px")
	; References, External Links, See Also, etc. collapsibles
	(".sectiontogglecheckbox"
	 :display "none")
	(".sectiontogglelabel"
	 :border-bottom (str:concat "20px solid" theme:primary)
	 :border-left "20px solid transparent"
	 :border-right "20px solid transparent"
	 :height "0px"
	 :width "0px"
	 :float "right"
	 :margin-right "10px"
	)
    (".sectiontogglelabel:hover"
     :border-bottom-color theme:on-accent)
    (".sectiontogglelabel:active"
     :border-bottom-color theme:on-secondary)
    (".sectiontogglecheckbox:checked + .sectiontogglelabel"
     :border-top (str:concat "20px solid" theme:primary)
     :border-bottom "0px solid transparent")
    (".sectiontogglecheckbox:checked + .sectiontogglelabel:hover"
     :border-top-color theme:on-accent)
    (".sectiontogglecheckbox:checked + .sectiontogglelabel:active"
     :border-top-color theme:on-secondary)
	("h2:has(.sectiontogglecheckbox:checked) ~ .reflist, h2:has(.sectiontogglecheckbox:checked) ~ .refbegin, h2:has(.sectiontogglecheckbox:checked) ~ h3:has(+ .reflist), h2:has(.sectiontogglecheckbox:checked) ~ h3:has(+ .refbegin), h2:has(.sectiontogglecheckbox:checked) + ul:has(.citation, .external)"
	 :display "none")
      ))

;(defun collapsible-triangle (class side)
;  `((,class
;     :float "right"
;     :height 0
;     :margin-right "10px"
;     :width 0
;    )
;
;  )
;)

(defun remove-elements (elements)
  (mapc
    #'plump:remove-child
    (coerce elements 'list)))

(defun fix-wiki-links (document tag-name attr base-url fixer)
  (plump:traverse document
                  (lambda (element)
                    (plump:set-attribute element attr
                      (funcall fixer element attr base-url)))
                  :test (lambda (element)
                          (and (plump:element-p element)
                               (string= (plump:tag-name element) tag-name)
                               (plump:get-attribute element attr)))))

(defun fix-wiki-link (element attr base-url)
  (plump:set-attribute element attr
    (quri:render-uri
      (quri:merge-uris
        (quri:uri (plump:get-attribute element attr))
        base-url))))

(define-internal-scheme "wiki" #'wiki-scheme-handler)
