(defconst merlin-helm-packages '(helm-cmd-t helm-describe-modes helm-file-preview helm-flycheck helm-flyspell helm-git-files helm-helm-commands helm-ispell helm-lastpass helm-proc))

(defun merlin-helm/init-helm-cmd-t ()
  (use-package helm-cmd-t)
  )

(defun merlin-helm/init-helm-describe-modes ()
  (use-package helm-describe-modes
    :config
    (global-set-key (kbd "<f1> m") #'helm-describe-modes)
    )
  )

(defun merlin-helm/init-helm-file-preview ()
  (use-package helm-file-preview
    :config
    (helm-file-preview-mode 1))
  )
(defun merlin-helm/init-helm-flycheck ()
  (use-package helm-flycheck
    :config
    (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck)
    )
  )

(defun merlin-helm/init-helm-flyspell ()
  (use-package helm-flyspell
    :config
    (define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct)
    )
  )

(defun merlin-helm/init-helm-git-files ()
  (use-package helm-git-files)
  )

(defun merlin-helm/init-helm-helm-commands ()
  (use-package helm-helm-commands)
  )

(defun merlin-helm/init-helm-ispell ()
  (use-package helm-ispell)
  )

(defun merlin-helm/init-helm-lastpass ()
  (use-package helm-lastpass)
  )

(defun merlin-helm/init-helm-proc ()
  (use-package helm-proc)
  )
