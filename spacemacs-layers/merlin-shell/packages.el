(defconst merlin-shell-packages '(bash-completion shfmt))

(defun merlin-shell/init-bash-completion ()
  (use-package bash-completion
    :config
    (bash-completion-setup)
    )
  )

(defun merlin-shell/init-shfmt ()
  (use-package shfmt)
  )
