(defconst projectile-ripgrep-packages
  '(projectile-ripgrep)
  )

(defun projectile-ripgrep/init-projectile-ripgrep ()
  (use-package projectile-ripgrep)
  (with-eval-after-load 'helm
    (spacemacs/declare-prefix "sr" "projectile-ripgrep")
    (spacemacs/set-leader-keys "srp" 'projectile-ripgrep)
    (define-key projectile-command-map (kbd "s r") 'projectile-ripgrep)
    )
  )


