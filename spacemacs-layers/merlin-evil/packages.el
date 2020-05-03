(defconst merlin-evil-packages
  '(
    evil-collection
    evil-easymotion
    evil-embrace
    evil-ex-fasd
    evil-ex-shell-command
    evil-expat
    evil-extra-operator
    evil-fringe-mark
    evil-mark-replace
    evil-smartparens
    powerline-evil
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

(defun merlin-evil/init-evil-smartparens ()
  (use-package evil-smartparens
    :config
    (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
    )
  )

(defun merlin-evil/init-powerline-evil ()
  (use-package powerline-evil)
  )
