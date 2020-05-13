(defconst merlin-evil-packages
  '(
    evil-avy
    evil-collection
    evil-easymotion
    evil-embrace
    evil-ex-fasd
    evil-ex-shell-command
    evil-expat
    evil-extra-operator
    evil-fringe-mark
    evil-mark-replace
    evil-owl
    evil-smartparens
    evil-space
    powerline-evil
    )
  )

(defun merlin-evil/init-evil-avy ()
  (use-package evil-avy
    :config
    (evil-avy-mode)
    )
  )

(defun merlin-evil/init-evil-collection ()
  (use-package evil-collection
    :config
    (evil-collection-init)
    )
  )

(defun merlin-evil/init-evil-easymotion ()
  (use-package evil-easymotion)
  (evilem-default-keybindings "<backtab>")
  )

(defun merlin-evil/init-evil-embrace ()
  (use-package evil-embrace
    :config
    (add-hook 'org-mode-hook 'embrace-org-mode-hook)
    (evil-embrace-enable-evil-surround-integration)
    )
  )

(defun merlin-evil/init-evil-ex-fasd ()
  (use-package evil-ex-fasd)
  )

(defun merlin-evil/init-evil-ex-shell-command ()
  (use-package evil-ex-shell-command)
  )

(defun merlin-evil/init-evil-expat ()
  (use-package evil-expat)
  )

(defun merlin-evil/init-evil-extra-operator ()
  (use-package evil-ex-fasd
    :config
    (setq evil-extra-operator-eval-key (kbd "ge"))
    (global-evil-extra-operator-mode 1)
    )
  )

(defun merlin-evil/init-evil-fringe-mark ()
  (use-package evil-fringe-mark)
  :config
  (global-evil-fringe-mark-mode)
  ;; Display special marks
  (setq-default evil-fringe-mark-show-special t)
  )

(defun merlin-evil/init-evil-mark-replace ()
  (use-package evil-mark-replace)
  )



(defun merlin-evil/init-evil-owl ()
  (use-package evil-owl
    :config
    ;; (setq evil-owl-display-method 'posframe
    ;;       evil-owl-extra-posframe-args '(:width 50 :height 20)
    ;;       evil-owl-max-string-length 50)
    (setq evil-owl-max-string-length 500)
    (add-to-list 'display-buffer-alist
                 '("*evil-owl*"
                   (display-buffer-in-side-window)
                   (side . bottom)
                   (window-height . 0.3)))
    (evil-owl-mode)
    )
  )

(defun merlin-evil/init-evil-smartparens ()
  (use-package evil-smartparens
    :config
    (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
    )
  )

(defun merlin-evil/init-evil-space ()
  (use-package evil-space
    :config
    (evil-space-mode)
    )
  )

(defun merlin-evil/init-powerline-evil ()
  (use-package powerline-evil)
  )
