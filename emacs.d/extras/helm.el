(use-package helm
;;  :straight t
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (setq helm-input-idle-delay               0.01
	helm-reuse-last-window-split-state  t
	helm-always-two-winows             t
	helm-split-window-inside-p          nil
	helm-commands-usin-fram                   '(completion-at-point helm-imenu helm-imenu-in-all-buffers)
	helm-actions-inherit-frame-settnigs       t 
	helm-use-frame-when-more-than-two-windows t
	helm-frame-background-color               "DarkSlateGrey"
	)
  :hook (evil-mode . helm-evil-markers-toggle)
  )

;; (add-to-list 'helm-completion-styles-alist '(switch-to-buffer . helm-fuzzy))

(if (boundp 'completions-detailed)
    (setq completions-detailed t)
  ;; Emacs-27<
  (setq helm-completions-detailed t))

;; (helm-top-poll-mode 1)


(require 'helm-dictionary)
(require 'helm-descbinds)
(helm-descbinds-mode)
