(defconst merlin-leading-spaces-packages '(highlight-leading-spaces))

(defun merlin-leading-spaces/init-highlight-leading-spaces ()
  (use-package highlight-leading-spaces
    :ensure t
    :defer t
    :init (add-hook 'prog-mode-hook 'highlight-leading-spaces-mode)
    )
  )
