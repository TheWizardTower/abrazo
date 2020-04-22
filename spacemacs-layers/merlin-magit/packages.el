(defconst merlin-magit-packages '(git-ps1-mode magit-filenotify))

(defun merlin-magit/init-git-ps1-mode()
  (use-package git-ps1-mode
    :config
    (git-ps1-mode t)
    )
  )

(defun merlin-magit/init-magit-filenotify ()
  (use-package magit-filenotify
    :config
    (add-hook 'magit-status-mode-hook 'magit-filenotify-mode)
    )
  )
