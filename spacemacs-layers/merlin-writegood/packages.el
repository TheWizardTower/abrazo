(defconst merlin-writegood-packages '(writegood-mode artbollocks-mode))

(defun merlin-writegood/init-writegood-mode ()
  (use-package writegood-mode
    :bind
    ("C-c g g" . 'writegood-mode)
    ("C-c g l" . 'writegood-grade-level)
    ("C-c g e" . 'writegood-reading-ease)
    )
  :config
  (add-hook 'text-mode-hook 'writegood-mode)
  )

(defun merlin-writegood/init-artbollocks-mode ()
  (use-package artbollocks-mode
                :config
                (add-hook 'text-mode-hook 'artbollocks-mode)
                )
  )
