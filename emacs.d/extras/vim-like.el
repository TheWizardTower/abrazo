;;; Emacs Bedrock
;;;
;;; Extra config: Vim emulation

;;; Usage: Append or require this file from init.el for bindings in Emacs.

;;; Contents:
;;;
;;;  - Core Packages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Core Packages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq evil-want-keybinding nil)

;; Evil: vi emulation
(use-package evil
  :ensure t

  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)

  ;; Enable this if you want C-u to scroll up, more like pure Vim
  ;; (setq evil-want-C-u-scroll t)

  :config
  (evil-mode)

  ;; Configuring initial major mode for some modes
  (evil-set-initial-state 'vterm-mode 'emacs))

(use-package evil-args)

;; bind evil-args text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; bind evil-forward/backward-args
(define-key evil-normal-state-map "L" 'evil-forward-arg)
(define-key evil-normal-state-map "H" 'evil-backward-arg)
(define-key evil-motion-state-map "L" 'evil-forward-arg)
(define-key evil-motion-state-map "H" 'evil-backward-arg)

;; bind evil-jump-out-args
(define-key evil-normal-state-map "K" 'evil-jump-out-args)

;;; evil-escape configuration
(setq-default evil-escape-key-sequence "hl")

(use-package beacon
  :init
  (beacon-mode 1))

(use-package evil-better-visual-line
  :ensure t
  :config
  (evil-better-visual-line-on))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-commentary
  :init
  (evil-commentary-mode)
  )


(use-package evil-easymotion
  :init
  (evilem-default-keybindings "SPC")
  )

(use-package evil-extra-operator
  :init
  (global-evil-extra-operator-mode 1)
  )

(use-package evil-expat)

(use-package evil-fringe-mark
  :init
  (setq-default left-fringe-width 16)
  (setq-default left-margin-width 2)
  (setq-default evil-fringe-mark-side 'left-fringe)
  (setq-default evil-fringe-mark-margin 'left-margin)
  (setq evil-fringe-mark-show-special t)
  (global-evil-fringe-mark-mode)
  )

(use-package evil-goggles
  :init
  (evil-goggles-mode)
  )

(use-package evil-indent-plus
  :init
  (evil-indent-plus-default-bindings)
  )

(use-package evil-leader
  :init
  (global-evil-leader-mode)
  )

(use-package evil-lion
  :init
  (evil-lion-mode)
  )
;;; (evil-lisp-state-leader ", l")

(use-package evil-matchit
  :init
  (global-evil-matchit-mode 1)
  )

(use-package evil-traces
  :config
  (evil-traces-use-diff-faces) ; if you want to use diff's faces
  (evil-traces-mode))

(use-package evil-space
  :config
  (evil-space-mode))

(use-package evil-smartparens
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  )

(use-package evil-quickscope
  :config
  (global-evil-quickscope-always-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  )
