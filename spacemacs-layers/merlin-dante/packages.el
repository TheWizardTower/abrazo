(defconst merlin-dante-packages '(dante))

(defun merlin-dante/init-dante ()
  (use-package dante
    :ensure t
    :after haskell-mode
    :commands 'dante-mode
    :init
    (add-hook 'haskell-mode-hook 'flycheck-mode)
    ;; OR:
    ;; (add-hook 'haskell-mode-hook 'flymake-mode)
    (add-hook 'haskell-mode-hook 'dante-mode)
    (add-hook 'dante-mode-hook
               '(lambda () (flycheck-add-next-checker 'haskell-dante
                                                      '(warning . haskell-hlint))))
    )
  )
