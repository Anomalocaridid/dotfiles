;;;; config.lisp
;;;; main config file for Nyxt

;;; misc settings
(define-configuration browser
   ;; never restore session
  ((restore-session-on-startup-p nil)
   ;; set external editor
   (external-editor-program '("handlr" "open"))))

;;; enable default modes
(define-configuration buffer
  ((default-modes (append '(nyxt/mode/blocker:blocker-mode
                            nyxt/mode/reduce-tracking:reduce-tracking-mode
                            nyxt/mode/password:password-mode
                            nyxt/mode/vi:vi-normal-mode)
                          %slot-value%))))

;;; password manager config
(defmethod initialize-instance :after
           ((interface password:keepassxc-interface)
            &key &allow-other-keys)
  (setf (password:password-file interface) "/home/anomalocaris/Sync/Keepass Databases/Personal.kdbx"
        (password:yubikey-slot interface) "2"))

(define-configuration nyxt/mode/password:password-mode
  ((nyxt/mode/password:password-interface
    (make-instance 'password:keepassxc-interface))))

;;; load all files in root of config.d
(eval 
  `(define-nyxt-user-system-and-load nyxt-user/config
    :components ,(mapcar (lambda (path)
                           (enough-namestring
                             path
                             (files:expand *config-file*))) 
                         (directory
                           (merge-pathnames
                             "config.d/*.lisp"
                             (uiop:pathname-directory-pathname
                               (files:expand *config-file*)))))))
