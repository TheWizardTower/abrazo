(defconst merlin-jq-packages '(jq-format jq-mode))

(defun merlin-jq/init-jq-format ()
  (use-package jq-format
    :config
    (spacemacs/set-leader-keys-for-major-mode 'json-mode "F" 'jq-format-json-buffer)
    :demand t
    :after json-mode)
  )

(defun merlin-jq/init-jq-mode ()
  (use-package jq-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode))
    (with-eval-after-load 'json-mode
      (define-key json-mode-map (kbd "C-c C-j") #'jq-interactively))
    )
  )
