(defconst ido-vertical-mode-packages
  '(ido-vertical-mode))


(defun ido-vertical-mode/init-ido-vertical-mode ()
     (use-package ido-vertical-mode)
     (ido-mode t)
     (ido-vertical-mode t)
     )
