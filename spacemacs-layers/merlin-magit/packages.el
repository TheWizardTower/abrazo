(defconst merlin-magit-packages '(magit-circleci magit-filenotify magit-find-file magit-org-todos magit-todos mo-git-blame travis))

(defun merlin-magit/init-magit-circleci ()
  (use-package magit-circleci
    :config
    ;;; (setq magit-circleci-token "XXXXXXXX") ;; Put this in a local secrets.el file.
    )
  )

(defun merlin-magit/init-magit-filenotify ()
  (use-package magit-filenotify
    :config
    (add-hook 'magit-status-mode-hook 'magit-filenotify-mode)
    )
  )

(defun merlin-magit/init-magit-find-file ()
  (use-package magit-find-file)
  )

(defun merlin-magit/init-magit-org-todos ()
  (use-package magit-org-todos
    :config
    (magit-org-todos-autoinsert)
    (magit-add-section-hook
     'magit-status-sections-hook
     'magit-org-todos-insert-org-todos
     'magit-insert-staged-changes
     t)
    )
  )

(defun merlin-magit/init-magit-todos ()
  (use-package magit-todos
    :config
    (add-hook 'magit-mode-hook 'magit-todos-mode)
    )
  )

(defun merlin-magit/init-mo-git-blame ()
  (use-package mo-git-blame
    :config
    (autoload 'mo-git-blame-file "mo-git-blame" nil t)
    (autoload 'mo-git-blame-current "mo-git-blame" nil t)
    )
  )

(defun merlin-magit/init-travis ()
  (use-package travis)
  )
