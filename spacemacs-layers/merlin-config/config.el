;; Set goto-line shortcut.
(define-key global-map "\M-g" 'goto-line)

;; Align with spaces only
(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)
(global-unset-key (kbd "C-x i"))
(global-set-key (kbd "C-x i") 'align-regexp)


(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)
     (company-flx-mode +1)))

(define-key 'help-command (kbd "C-l") 'helm-locate-library)


;;; This seems to be the one place where Ido wins out over helm. Helm plus Tramp bring Emacs to a crawl.
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-vertical-show-count t)
(global-set-key (kbd "C-c C-f") 'counsel-find-file)

(global-set-key (kbd "C-x G") 'magit-status)


                                        ; To disable slowdown when editing remote files.
(add-hook 'find-file-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (setq-local projectile-mode-line "Projectile[remote]"))))

(global-unset-key (kbd "C-x o"))
(global-set-key (kbd "C-x o") 'ace-window)


(eval-after-load "helm"
  '(define-key helm-map (kbd "C-'") 'ace-jump-helm-line))
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode-pop-mark)
(put 'set-goal-column 'disabled nil)
