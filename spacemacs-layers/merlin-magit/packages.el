(defconst merlin-magit-packages '(magit-filenotify))

(defun merlin-magit/init-magit-filenotify ()
  (use-package magit-filenotify
    :config
    (add-hook 'magit-status-mode-hook 'magit-filenotify-mode)
    )
  )
