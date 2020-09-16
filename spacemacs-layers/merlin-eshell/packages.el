(defconst merlin-eshell-packages
  '(eshell-fringe-status
    eshell-git-prompt))

(defun merlin-eshell/init-eshell-fringe-status ()
  (use-package eshell-fringe-status)
  (add-hook 'eshell-mode-hook 'eshell-fringe-status-mode))

(defun merlin-eshell/init-eshell-git-prompt ()
  (use-package eshell-git-prompt))
