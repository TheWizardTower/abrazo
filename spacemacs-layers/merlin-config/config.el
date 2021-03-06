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

(define-key 'help-command (kbd "C-l") 'helm-locate-library)

;;; This seems to be the one place where Ido wins out over helm. Helm plus Tramp bring Emacs to a crawl.
;; (setq ido-vertical-define-keys 'C-n-and-C-p-only)
;; (setq ido-vertical-show-count t)
;; (global-set-key (kbd "C-c C-f") 'counsel-find-file)

; To disable slowdown when editing remote files.
(add-hook 'find-file-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (setq-local projectile-mode-line "Projectile[remote]"))))

(global-unset-key (kbd "C-x o"))
(global-set-key (kbd "C-x o") 'ace-window)

(spacemacs/set-leader-keys "ps" 'projectile-ripgrep)

(eval-after-load "helm"
  '(define-key helm-map (kbd "C-'") 'ace-jump-helm-line))
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode-pop-mark)
(put 'set-goal-column 'disabled nil)


;; Configure some modes for file names that aren't standard.
(add-to-list 'auto-mode-alist '("alias\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("bashrc\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("bash_profile\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("screenrc\\'" . conf-space-mode))
(add-to-list 'auto-mode-alist '("emacs\\'" . emacs-lisp-mode))

(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((haskell . t)))

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(defun run-command-on-current-file ()
  "Run a shell command on the file behind the current buffer, then reload."
  (interactive)
  (message buffer-file-name)
  (when buffer-file-name
    (shell-command
     (format
      "%s %s"
      (read-shell-command "Shell command on file: ")
      (file-truename buffer-file-name)
      )
     "shell-command: output"
     "shell-command: Error"
     )
    )
  )

;; This ugly monstrosity is how "C-!" is interpreted under WSL.
(global-set-key (kbd "M-[ 1 ; 6 q") 'run-command-on-current-file)
(global-set-key (kbd "C-!") 'run-command-on-current-file)
(spacemacs/set-leader-keys "bx" 'run-command-on-current-file)

(setq scheme-program-name "guile")
(setq vc-follow-symlinks t)

(defun merlin-sort-list-temporary (list)
  (defun sort-util (a b)
    (string<
     (symbol-name a)
     (symbol-name b)
     )
    )
  (seq-sort-by sort-util list)
  )
