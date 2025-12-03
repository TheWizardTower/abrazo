;;; Emacs Bedrock
;;;
;;; Extra config: Vim emulation

;;; Usage: Append or require this file from init.el for Vim bindings in Emacs.

;;; Contents:
;;;
;;;  - Core Packages
;;;  - Evil Extensions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Core Packages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Must be set BEFORE loading evil
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)  ; Essential for Vim-like scrolling

;; Evil: vi emulation
(use-package evil
  :demand t
  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  ;; Set initial states for specific modes
  (evil-set-initial-state 'vterm-mode 'emacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Evil Extensions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil-collection
  :after evil
  :demand t
  :config
  (evil-collection-init))

(use-package evil-args
  :after evil
  :config
  ;; Bind evil-forward/backward-args
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg)
  ;; Bind evil-jump-out-args
  (define-key evil-normal-state-map "K" 'evil-jump-out-args))

;;; evil-escape configuration

(use-package evil-escape
  :after evil
  :demand t
  :config
  (setq-default evil-escape-key-sequence "hu")
  (setq-default evil-escape-delay 0.1)
  (evil-escape-mode)
  :bind (:map evil-insert-state-map
              ("C-c C-g" . evil-escape)))

(use-package beacon
  :demand t
  :init
  (beacon-mode 1))

(use-package evil-better-visual-line
  :after evil
  :demand t
  :config
  (evil-better-visual-line-on))

(use-package evil-commentary
  :after evil
  :demand t
  :init
  (evil-commentary-mode))

(use-package evil-surround
  :after evil
  :demand t
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :after evil
  :demand t
  :init
  (global-evil-matchit-mode 1))

(use-package evil-lion
  :after evil
  :demand t
  :init
  (evil-lion-mode))

(use-package evil-goggles
  :after evil
  :demand t
  :init
  (evil-goggles-mode))

(use-package evil-indent-plus
  :after evil
  :config
  (evil-indent-plus-default-bindings))

(use-package evil-leader
  :after evil
  :demand t
  :init
  (global-evil-leader-mode))

(use-package evil-traces
  :after evil
  :demand t
  :config
  (evil-traces-use-diff-faces) ; if you want to use diff's faces
  (evil-traces-mode))

(use-package evil-space
  :after evil
  :demand t
  :config
  (evil-space-mode))

(use-package evil-smartparens
  :after (evil smartparens)
  :hook (smartparens-enabled . evil-smartparens-mode))

(use-package evil-quickscope
  :after evil
  :demand t
  :config
  (global-evil-quickscope-always-mode 1))

(use-package evil-visualstar
  :after evil
  :demand t
  :config
  (global-evil-visualstar-mode t))

(use-package evil-string-inflection
  :after evil)

(use-package evil-terminal-cursor-changer
  :after evil
  :if (not (display-graphic-p))
  :config
  (evil-terminal-cursor-changer-activate))

(use-package evil-vimish-fold
  :after evil
  :demand t
  :config
  (global-evil-vimish-fold-mode))

(use-package evil-visual-mark-mode
  :after evil
  :demand t
  :config
  (evil-visual-mark-mode))

(use-package evil-visual-replace
  :after evil
  :config
  (evil-visual-replace-visual-bindings))
