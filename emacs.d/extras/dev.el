;;; Emacs Bedrock
;;;
;;; Extra config: Development tools

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; It is **STRONGLY** recommended that you use the base.el config if you want to
;;; use Eglot. Lots of completion things will work better.
;;;
;;; This will try to use tree-sitter modes for many languages. Please run
;;;
;;;   M-x treesit-install-language-grammar
;;;
;;; Before trying to use a treesit mode.

;;; Contents:
;;;
;;;  - Built-in config for developers
;;;  - Version Control
;;;  - Common file types
;;;  - Eglot, the built-in LSP client for Emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  ;; Treesitter config

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (rust-mode . rust-ts-mode)
          (python-mode . python-ts-mode)))
  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package magit-file-icons)
(use-package magit-diff-flycheck)
(use-package magit-filenotify)
(use-package magit-todos)
(use-package magit-find-file
  :config
  (require 'magit-find-file) ;; if not using the ELPA package
  (global-set-key (kbd "C-c g") 'magit-find-file-completing-read)
  )

(use-package magit-commit-mark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

;; Emacs ships with a lot of popular programming language modes. If it's not
;; built in, you're almost certain to find a mode for the language you're
;; looking for with a quick Internet search.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for selected modes
  ;; :hook
  ;; (((python-mode ruby-mode elixir-mode) . eglot))

  :custom
  (eglot-send-changes-idle-time 0.1)

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  (add-to-list 'eglot-server-programs
               '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  )

(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  )

;; Rust configuration
(load-file (expand-file-name "extras/rust.el" user-emacs-directory))

;; Haskell configuration
(load-file (expand-file-name "extras/haskell.el" user-emacs-directory))

;; Helm configuration
(load-file (expand-file-name "extras/helm.el" user-emacs-directory))

;; Typescript. Because life isn't hard enough, right?
(load-file (expand-file-name "extras/typescript.el" user-emacs-directory))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  )

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

(use-package projectile-codesearch)
(use-package projectile-ripgrep)
(use-package projectile-sift)
(use-package projectile-speedbar)
(use-package projectile-variable)

(use-package line-reminder
  :config
  (global-line-reminder-mode)
  )
(use-package watch-buffer)

(use-package shfmt
  :config
  (add-hook 'sh-mode-hook 'shfmt-on-save-mode)
  )

(use-package ssh-agency)

(use-package ssh-config-mode
  :config
  (add-to-list 'auto-mode-alist '("/\\.ssh/config\\(\\.d/.*\\.conf\\)?\\'" . ssh-config-mode))
  (add-to-list 'auto-mode-alist '("/sshd?_config\\(\\.d/.*\\.conf\\)?\\'"  . ssh-config-mode))
  (add-to-list 'auto-mode-alist '("/known_hosts\\'"       . ssh-known-hosts-mode))
  (add-to-list 'auto-mode-alist '("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock)
  )
;; (xhair-mode)


(use-package yasnippet
  :init
  (yas-global-mode 1)
  )

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  )

(use-package company-dict)
(use-package company-emacs-eclim)
(use-package company-irony)
(use-package company-irony-c-headers)
(use-package company-prescient
  :config
  (company-prescient-mode)
  )
(use-package company-quickhelp
  :config
  (company-quickhelp-mode)
  )
(use-package company-quickhelp-terminal)
(use-package company-shell)
(use-package company-spell)

(use-package company-fuzzy
  :hook (company-mode . company-fuzzy-mode)
  :init
  (global-company-fuzzy-mode 1)
  (setq company-fuzzy-sorting-backend 'flx
        company-fuzzy-reset-selection t
        company-fuzzy-prefix-on-top nil
        company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@")))

(use-package discover
  :config
  (global-discover-mode)
  )

(use-package discover-my-major)

(use-package flycheck-aspell
  :config
  ;; If you want to check TeX/LaTeX/ConTeXt buffers
  (add-to-list 'flycheck-checkers 'tex-aspell-dynamic)
  ;; If you want to check Markdown/GFM buffers
  (add-to-list 'flycheck-checkers 'markdown-aspell-dynamic)
  ;; If you want to check HTML buffers
  (add-to-list 'flycheck-checkers 'html-aspell-dynamic)
  ;; If you want to check XML/SGML buffers
  (add-to-list 'flycheck-checkers 'xml-aspell-dynamic)
  ;; If you want to check Nroff/Troff/Groff buffers
  (add-to-list 'flycheck-checkers 'nroff-aspell-dynamic)
  ;; If you want to check Texinfo buffers
  (add-to-list 'flycheck-checkers 'texinfo-aspell-dynamic)
  ;; If you want to check comments and strings for C-like languages
  (add-to-list 'flycheck-checkers 'c-aspell-dynamic)
  ;; If you want to check message buffers
  (add-to-list 'flycheck-checkers 'mail-aspell-dynamic)
  )

(use-package flycheck-popup-tip
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)
  (flycheck-pos-tip-mode)
  )

(use-package flycheck-status-emoji
  :config
  (flycheck-status-emoji-mode)
  )

(use-package flycheck-yamllint
  :config
  (flycheck-yamllint-setup)
  )

(use-package flyspell-correct)
