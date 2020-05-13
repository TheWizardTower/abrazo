(defconst merlin-doom-packages '(anzu doom-themes doom-modeline))

(defun merlin-doom/init-anzu ()
  (use-package anzu
    ;; :after-call isearch-mode
    )
  )

(defun merlin-doom/init-doom-themes ()
  (use-package doom-themes)
  )

(defun merlin-doom/init-doom-modeline ()
  (use-package doom-modeline
    :ensure t
    :hook (after-init . doom-modeline-mode)
    :hook (doom-modeline-mode . size-indication-mode) ; filesize in modeline
    :hook (doom-modeline-mode . column-number-mode)   ; cursor column in modeline
    :init
    (doom-modeline-mode 1)
    (setq doom-modeline-bar-width 3
          doom-modeline-github nil
          doom-modeline-mu4e nil
          doom-modeline-persp-name nil
          ;; doom-modeline-minor modes nil
          doom-modeline-major-mode-icon nil
          doom-modeline-buffer-file-name-style 'relative-from-project
          )
    (add-hook 'doom-change-font-size-hook #'+modeline-resize-for-font-h)
    (add-hook 'doom-load-theme-hook #'doom-modeline-refresh-bars)
    (add-hook '+doom-dashboard-mode-hook #'doom-modeline-set-project-modeline)
    (add-hook 'magit-mode-hook
               (defun +modeline-hide-in-non-status-buffer-h ()
                 "Show minimal modeline in magit-status buffer, no modeline elsewhere."
                 (if (eq major-mode 'magit-status-mode)
                     (doom-modeline-set-vcs-modeline)
                   (hide-mode-line-mode))))

    ;; Some functions modify the buffer, causing the modeline to show a false
    ;; modified state, so force them to behave.
    ;; (defadvice +modeline--inhibit-modification-hooks-a (orig-fn &rest args)
    ;;   :around #'ws-butler-after-save
    ;;   (with-silent-modifications (apply orig-fn args)))
    )
  )
