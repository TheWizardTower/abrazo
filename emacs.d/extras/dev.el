;;; Emacs Bedrock
;;;
;;; Extra config: Development tools

;;; Usage: Append or require this file from init.el for software development packages.

;;; Contents:
;;;
;;;  - Built-in config for developers
;;;  - Version Control
;;;  - Common file types
;;;  - LSP Mode (Language Server Protocol)
;;;  - Project Management
;;;  - Development Utilities

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :demand t
  :config
  ;; Treesitter config - prefer treesitter modes
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
  (prog-mode . electric-pair-mode))

(use-package treesit-auto
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch)))

(use-package magit-diff-flycheck)
(use-package magit-filenotify)
(use-package magit-todos)
(use-package magit-find-file
  :demand t
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
  :hook (markdown-mode . visual-line-mode))

(use-package yaml-mode
  :hook (yaml-mode . prog-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package json-mode)

(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.tpl\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   LSP Mode (Language Server Protocol)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  ;; Performance tuning
  (lsp-idle-delay 0.500)
  (lsp-log-io nil)
  (lsp-completion-provider :capf)
  (lsp-prefer-flymake nil)
  (lsp-keep-workspace-alive nil)

  ;; Rust-specific settings
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-inlay-hint-enable t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)

  :hook ((haskell-mode . lsp)
         (scala-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-lens-mode))

  :config
  ;; Haskell server config
  (add-to-list 'lsp-server-programs
               '(haskell-mode . ("haskell-language-server-wrapper" "--lsp"))))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-show-hover t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Project Management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :demand t
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  :bind-keymap
  (("s-p" . projectile-command-map)
   ("C-c p" . projectile-command-map)))

(use-package projectile-ripgrep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Development Utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-window
  :bind (("M-o" . ace-window)))

(use-package dirvish
  :demand t
  :init
  (dirvish-override-dired-mode))

(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :config
  (yas-reload-all))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package flycheck-inline
  :hook (flycheck-mode . flycheck-inline-mode))

(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("C" (astyle "--mode=c"))
                  ("Shell" (shfmt "-i" "4" "-ci"))
                  ("Rust" (rustfmt))
                  ("Terraform" (terraform-fmt))
                  ("Scala" (scalafmt))
                  ("Haskell" (fourmolu))
                  ("Python" (black))
                  ("JavaScript" (prettier))
                  ("TypeScript" (prettier))
                  ("Go" (gofmt)))))

(use-package shfmt
  :hook (sh-mode . shfmt-on-save-mode))

(use-package ssh-config-mode
  :config
  (add-to-list 'auto-mode-alist '("/\\.ssh/config\\(\\.d/.*\\.conf\\)?\\'" . ssh-config-mode))
  (add-to-list 'auto-mode-alist '("/sshd?_config\\(\\.d/.*\\.conf\\)?\\'" . ssh-config-mode))
  (add-to-list 'auto-mode-alist '("/known_hosts\\'" . ssh-known-hosts-mode))
  (add-to-list 'auto-mode-alist '("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock))

(use-package line-reminder
  :demand t
  :config
  (global-line-reminder-mode))

(use-package watch-buffer)

(use-package discover
  :demand t
  :config
  (global-discover-mode))

(use-package discover-my-major)

(use-package dashboard
  :demand t
  :config
  (dashboard-setup-startup-hook))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode))

;; Builtin whitespace mode
(require 'whitespace)
(add-hook 'prog-mode-hook 'whitespace-mode)

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

;; Eat terminal emulator
(quelpa '(eat :fetcher git
              :url "https://codeberg.org/akib/emacs-eat"
              :files ("*.el" ("term" "term/*.el") "*.texi"
                      "*.ti" ("terminfo/e" "terminfo/e/*")
                      ("terminfo/65" "terminfo/65/*")
                      ("integration" "integration/*")
                      (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package bank-buddy)
(with-eval-after-load 'bank-buddy
  (add-hook 'org-mode-hook 'bank-buddy-cat-maybe-enable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Language-specific configurations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load language-specific configs
(load-file (expand-file-name "extras/rust.el" user-emacs-directory))
(load-file (expand-file-name "extras/haskell.el" user-emacs-directory))
(load-file (expand-file-name "extras/typescript.el" user-emacs-directory))
(load-file (expand-file-name "extras/scala.el" user-emacs-directory))
(load-file (expand-file-name "extras/terraform.el" user-emacs-directory))
(load-file (expand-file-name "extras/copilot.el" user-emacs-directory))
