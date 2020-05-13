(defconst merlin-crux-packages '(crux))

(defun merlin-crux/init-crux ()
  (use-package crux
    :bind
    ("C-k" . crux-smart-kill-line)
    ("C-c I" . crux-find-user-init-file)
    ("C-c d" . crux-duplicate-current-line-or-region)
    ;;("C-c e" . crux-eval-and-replace)
    ("C-c f k" . crux-delete-file-and-buffer)
    ("C-c f m" . crux-rename-file-and-buffer)
    ("C-x 4 t" . crux-transpose-windows)
    )
  )
