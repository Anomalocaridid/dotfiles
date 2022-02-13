(define-configuration buffer
  ((default-modes (append '(nyxt::vi-normal-mode) %slot-default%))))
(define-configuration browser
  ((default-new-buffer-url (quri:uri "nyxt:dashboard"))))
