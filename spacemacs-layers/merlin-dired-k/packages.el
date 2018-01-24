
(defconst merlin-dired-k-packages '(dired-k))

(defun merlin-dired-k/init-dired-k ()
  (use-package dired-k
    :config
    ;; always execute dired-k when dired buffer is opened
    (add-hook 'dired-mode-hook 'dired-k)
    (add-hook 'dired-initial-position-hook 'dired-k)
    (add-hook 'dired-after-readin-hook #'dired-k-no-revert)
    )
  )
