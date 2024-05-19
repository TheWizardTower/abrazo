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
  ;(setq evil-want-C-u-scroll t)

  :config
  (evil-mode)

  ;; Configuring initial major mode for some modes
  (evil-set-initial-state 'vterm-mode 'emacs))

  (beacon-mode 1)

    ;; locate and load the package
    (add-to-list 'load-path "path/to/evil-args")
    (require 'evil-args)

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

(use-package evil-better-visual-line
  :ensure t
  :config
  (evil-better-visual-line-on))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(evil-commentary-mode)

(evilem-default-keybindings "SPC")

(global-evil-extra-operator-mode 1)

(require 'evil-expat)

(global-evil-fringe-mark-mode)

(evil-goggles-mode)

(evil-indent-plus-default-bindings)

(global-evil-leader-mode)

(evil-lion-mode)

;;; (evil-lisp-state-leader ", l")

(global-evil-matchit-mode 1)
