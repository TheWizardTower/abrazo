(defconst merlin-ts-packages '(ts-comint tss))

(defun merlin-ts/init-ts-comint ()
  (use-package ts-comint
    :config
    (add-hook 'typescript-mode-hook
              (lambda ()
                (local-set-key (kbd "C-x C-e") 'ts-send-last-sexp)
                (local-set-key (kbd "C-M-x") 'ts-send-last-sexp-and-go)
                (local-set-key (kbd "C-c b") 'ts-send-buffer)
                (local-set-key (kbd "C-c C-b") 'ts-send-buffer-and-go)
                (local-set-key (kbd "C-c l") 'ts-load-file-and-go)
                )
              )
    )
  )

(defun merlin-ts/init-tss ()
  (use-package tss
    :config
    (setq tss-popup-help-key "C-:")
    (setq tss-jump-to-definition-key "C->")
    (setq tss-implement-definition-key "C-c i")
    (tss-config-default)
    )
  )
