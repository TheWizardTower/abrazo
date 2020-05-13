(defconst merlin-flycheck-packages '(flycheck-checkbashisms flycheck-clang-analyzer flycheck-clang-analyzer flycheck-clangcheck flycheck-clang-tidy flycheck-indicator flycheck-mypy flycheck-popup-tip flycheck-pycheckers))

(defun merlin-flycheck/init-flycheck-checkbashisms ()
  (use-package flycheck-checkbashisms
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-checkbashisms-setup)
    )
  )

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

(defun merlin-flycheck/init-flycheck-clang-analyzer ()
  (use-package flycheck-clang-analyzer
    :ensure t
    :after flycheck
    :config (flycheck-clang-analyzer-setup)
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

(defun merlin-flycheck/init-flycheck-indicator ()
  (use-package flycheck-indicator
    :hook (flycheck-mode . flycheck-indicator-mode)
    )
  )

(defun merlin-flycheck/init-flycheck-mypy ()
  (use-package flycheck-mypy)
  )

(defun merlin-flycheck/init-flycheck-popup-tip ()
  (use-package flycheck-popup-tip
    :after
    flycheck
    :config
    (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)
    )
  )

(defun merlin-flycheck/init-flycheck-pycheckers ()
  (use-package flycheck-pycheckers
    :after flycheck
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)
    )
  )
