(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
	      ("M-j" . lsp-ui-imenu)
	      ("M-?" . lsp-find-references)
	      ("C-c C-c l" . flycheck-list-errors)
	      ("C-c C-c a" . lsp-execute-code-action)
	      ("C-c C-c r" . lsp-reame)
	      ("C-c C-c q" . lsp-workspace-restart)
	      ("C-c C-c Q" . lsp-workspace-shutdown)
	      ("C-c C-c s" . lsp-rust-analyzer-status)
	      )
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)

  (defun rk/rustic-mode-hook ()
    ;; so that run C-c C-c C-r works without having to confirm, but don't try to
    ;; save rust bufferst that are not file visiting. Once
    ;;  https://github.com/brotzeit/rustic/issues/253 has been resolved this should
    ;; no longer be necessary
    (when buffer-file-name
      (setq-local buffer-save-without-query t))
    (add-hook 'before-save-hook 'lsp-format-buffer nil t))
  )



(use-package toml-mode)

(use-package rust-playground)

(use-package flycheck-rust
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  )

(use-package cargo)
(use-package cargo-mode)
(use-package cargo-transient)
