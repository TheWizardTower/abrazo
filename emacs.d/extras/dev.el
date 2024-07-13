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


;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :config
  (defun marginalia-toggle ()
    (interactive)
    (mapc
     (lambda (x)
       (setcdr x (append (reverse (remq 'none
					(remq 'builtin (cdr x))))
			 '(builtin none))))
     marginalia-annotator-registry))
  (defun marginalia-use-builtin ()
    (interactive)
    (mapc
     (lambda (x)
       (setcdr x (cons 'builtin (remq 'builtin (cdr x)))))
     marginalia-annotator-registry))
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package yasnippet
  :init
  (yas-global-mode 1)
  )

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  )

(use-package company-fuzzy
  :hook (company-mode . company-fuzzy-mode)
  :init
  (global-company-fuzzy-mode 1)
  (setq company-fuzzy-sorting-backend 'flx
        company-fuzzy-reset-selection t
        company-fuzzy-prefix-on-top nil
        company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@")))
