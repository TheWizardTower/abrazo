(defconst merlin-flycheck-packages '(flycheck-clang-analyzer flycheck-clangcheck flycheck-clang-tidy flycheck-mypy flycheck-pycheckers))

(defun merlin-flycheck/init-flycheck-clang-analyzer ()
  (use-package flycheck-clang-analyzer
    :ensure t
    :after flycheck
    :config (flycheck-clang-analyzer-setup)
    )
  )

(defun my-select-clangcheck-for-checker ()
  "Select clang-check for flycheck's checker."
  (flycheck-set-checker-executable 'c/c++-clangcheck
                                   "/use/bin/clang-check")
  (flycheck-select-checker 'c/c++-clangcheck))

(defun merlin-flycheck/init-flycheck-clangcheck ()
  (use-package flycheck-clangcheck
    :config
    (add-hook 'c-mode-hook #'my-select-clangcheck-for-checker)
    (add-hook 'c++-mode-hook #'my-select-clangcheck-for-checker)
    (setq flycheck-clangcheck-analyze t)
    )
  )

(defun merlin-flycheck/init-flycheck-clang-tidy ()
  (use-package flycheck-clang-tidy
    :after flycheck
    :hook
    (flycheck-mode . flycheck-clang-tidy-setup)
    )
  )

(defun merlin-flycheck/init-flycheck-irony ()
  (use-package flycheck-irony
    :after flycheck
    :config
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
    )
  )

(defun merlin-flycheck/init-flycheck-mypy ()
  (use-package flycheck-mypy)
  )

(defun merlin-flycheck/init-flycheck-pycheckers ()
  (use-package flycheck-pycheckers
    :after flycheck
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)
    )
  )
