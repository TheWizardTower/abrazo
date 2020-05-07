(defconst merlin-dired-packages
  '(
    dired-git
    dired-git-info
    dired-k
    dired-launch
    dired-narrow
    dired-quick-sort
    dired-rainbow
    dired-ranger
    dired-recent
    dired-rifle
    dired-rsync
    dired-toggle-sudo
    diredful
    fd-dired
    helm-dired-history
             )
  )

(defun merlin-dired/init-dired-git ()
  (use-package dired-git
    :config
    (add-hook 'dired-mode-hook 'dired-git-mode)
    )
  )

(defun merlin-dired/init-dired-git-info ()
  (use-package dired-git-info
    :config
    (with-eval-after-load 'dired
      (define-key dired-mode-map ")" 'dired-git-info-mode)
      )
    (setq dgi-auto-hide-details-p nil)
    )
  )

(defun merlin-dired/init-dired-k ()
  (use-package dired-k
    :config
    ;; (define-key dired-mode-map (kbd "K") 'dired-k)

    ;; You can use dired-k alternative to revert-buffer
    ;; (define-key dired-mode-map (kbd "g") 'dired-k)
    ;; always execute dired-k when dired buffer is opened
    (add-hook 'dired-mode-hook 'dired-k)
    (add-hook 'dired-initial-position-hook 'dired-k)
    (add-hook 'dired-after-readin-hook #'dired-k-no-revert)
    )
  )

(defun merlin-dired/init-dired-launch ()
  (use-package dired-launch
    :config
    (dired-launch-enable)
    )
  )

(defun merlin-dired/init-dired-narrow ()
  (use-package dired-narrow))

(defun merlin-dired/init-dired-quick-sort ()
  (use-package dired-quick-sort
    :config
    (dired-quick-sort-setup)
    )
  )

(defun merlin-dired/init-dired-rainbow ()
  (use-package dired-rainbow)
  )

(defun merlin-dired/init-dired-ranger ()
  (use-package dired-ranger)
  )

(defun merlin-dired/init-dired-recent ()
  (use-package dired-recent)
  )

(defun merlin-dired/init-dired-rifle ()
  (use-package dired-rifle)
  )

(defun merlin-dired/init-dired-rsync ()
  (use-package dired-rsync)
  )

(defun merlin-dired/init-diredful ()
  (use-package diredful)
  )

(defun merlin-dired/init-dired-toggle-sudo ()
  (use-package dired-toggle-sudo
    :config
    (define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)
    (eval-after-load 'tramp
      '(progn
         ;; Allow to use: /sudo:user@host:/path/to/file
         (add-to-list 'tramp-default-proxies-alist
                      '(".*" "\\`.+\\'" "/ssh:%h:")
                      )
         )
      )
    )
  )

(defun merlin-dired/init-evil-owl ()
  (use-packages evil-owl)
  )

(defun merlin-dired/init-fd-dired ()
  (use-package fd-dired
    :config
    (defun projectile-fd-dired ()
      (interactive)
      (fd-dired (projectile-project-root)
                (read-regexp "pattern to search for: ")
                )
      )
    )
  )

(defun merlin-dired/init-helm-dired-history ()
  (use-package helm-dired-history
    :config
    (add-to-list 'savehist-additional-variables 'helm-dired-history-variable)
    (savehist-mode 1)

    (with-eval-after-load 'dired
      (require 'helm-dired-history)
      ;; if you are using ido,you'd better disable ido for dired
      ;; (define-key (cdr ido-minor-mode-map-entry) [remap dired] nil) ;in ido-setup-hook
      (define-key dired-mode-map "," 'dired)
      )
    )
  )
