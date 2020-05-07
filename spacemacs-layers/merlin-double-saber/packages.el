(defconst merlin-double-saber-packages '(double-saber))

(defun merlin-double-saber/init-double-saber ()
  (use-package double-saber
    :config
    (with-eval-after-load "ripgrep"
      (add-hook 'ripgrep-search-mode-hook
                (lambda ()
                  (double-saber-mode)
                  (setq-local double-saber-start-line 5)
                  (setq-local double-saber-end-text "Ripgrep finished"))))

    (with-eval-after-load "grep"
      (add-hook 'grep-mode-hook
                (lambda ()
                  (double-saber-mode)
                  (setq-local double-saber-start-line 5)
                  (setq-local double-saber-end-text "Grep finished"))))

    (with-eval-after-load "ggtags"
      (add-hook 'ggtags-global-mode-hook
                (lambda ()
                  (double-saber-mode)
                  (setq-local double-saber-start-line 5)
                  (setq-local double-saber-end-text "Global found"))))

    (with-eval-after-load "ivy"
      (add-hook 'ivy-occur-grep-mode-hook
                (lambda ()
                  (double-saber-mode)
                  (setq-local double-saber-start-line 5))))
    (with-eval-after-load "helm"
      (add-hook 'projectile-ripgrep
                (lambda ()
                  (double-saber-mode)
                  (setq-local double-saber-start-line 5)
                  (setq-local double-saber-end-text "Ripgrep finished"))))
    )
  )
