(defmethod customize-instance ((input-buffer input-buffer) &key)
  (disable-modes '(nyxt/emacs-mode:emacs-mode) input-buffer)
  (enable-modes '(nyxt/vi-mode:vi-normal-mode) input-buffer))
