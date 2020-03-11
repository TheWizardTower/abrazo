(defconst merlin-writegood-packages '(writegood-mode artbollocks-mode))

(defun merlin-writegood/init-writegood-mode ()
  (use-package writegood-mode
    :config
    (global-set-key "\C-cg" 'writegood-mode)
    (global-set-key "\C-c\C-gg" 'writegood-grade-level)
    (global-set-key "\C-c\C-ge" 'writegood-reading-ease)
    )
  )

(defun merlin-writegood/init-artbollocks-mode ()
  (use-package artbollocks-mode
                :config
                (add-hook 'text-mode-hook 'artbollocks-mode)
                )
  )
