(defconst merlin-markdownfmt-packages '(markdownfmt))

(defun merlin-markdownfmt/init-markdownfmt ()
  (use-package markdownfmt
    :config
    (spacemacs/set-leader-keys-for-major-mode 'markdown-mode ",F" #'markdownfmt-format-buffer)
    )
  )
