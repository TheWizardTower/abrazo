(defconst merlin-evil-packages
  '(evil-collection
    evil-easymotion
    evil-embrace
    evil-ex-fasd
    evil-extra-operator
    evil-expat
    evil-ex-shell-command
    evil-goggles
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
    (evil-embrace-enable-evil-surround-integration)
    )
  )

(defun merlin-evil/init-evil-ex-fasd ()
  (use-package evil-ex-fasd)
  )

(defun merlin-evil/init-evil-extra-operator ()
  (use-package evil-ex-fasd
    :config
    (setq evil-extra-operator-eval-key (kbd "ge"))
    (global-evil-extra-operator-mode 1)
    )
  )

(defun merlin-evil/init-evil-expat ()
  (use-package evil-expat)
  )

(defun merlin-evil/init-evil-ex-shell-command ()
  (use-package evil-ex-shell-command)
  )

(defun merlin-evil/init-evil-goggles ()
  (use-package evil-goggles

    :ensure t
    :config
    (evil-goggles-mode)

    ;; optionally use diff-mode's faces; as a result, deleted text
    ;; will be highlighed with `diff-removed` face which is typically
    ;; some red color (as defined by the color theme)
    ;; other faces such as `diff-added` will be used for other actions
    (evil-goggles-use-diff-faces)
    (setq evil-goggles-pulse 't)
    (setq evil-goggles-duration 0.500) ;; default is 0.200
    (setq evil-ace-jump-active 't)
    ))
