(defconst merlin-helm-packages '(helm-file-preview helm-flycheck helm-flyspell helm-git-files helm-git-grep helm-helm-commands helm-ispell helm-lastpass helm-ls-git helm-proc))

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

(defun merlin-helm/init-helm-git-grep ()
  (use-package helm-git-grep
    ;; TODO: Find better keybinds.
    ;; :config
    ;; (global-set-key (kbd "C-c g") 'helm-git-grep)
    ;; ;; Invoke `helm-git-grep' from isearch.
    ;; (define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
    ;; ;; Invoke `helm-git-grep' from other helm.
    ;; (eval-after-load 'helm
    ;;   '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm))
    )
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

(defun merlin-helm/init-helm-ls-git ()
  (use-package helm-ls-git)
  )

(defun merlin-helm/init-helm-proc ()
  (use-package helm-proc)
  )

