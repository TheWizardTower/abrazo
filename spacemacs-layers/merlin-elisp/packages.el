(defconst merlin-elisp-packages '(elpl elsa))

(defun merlin-elisp/init-elpl ()
  (use-package elpl
    :config
    (define-key elpl-mode-map (kbd "C-c L") 'elpl-clean)
    (define-key elpl-mode-map (kbd "C-c '") 'elpl-edit)
    )
  )

(defun merlin-elisp/init-elsa ()
  (use-package elsa)
  )
