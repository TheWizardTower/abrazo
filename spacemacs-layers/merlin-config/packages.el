(defconst merlin-config-packages '(fortune-cookie modern-fringes))

(defun merlin-config/init-fortune-cookie ()
  (use-package fortune-cookie
    :config
    (setq fortune-cookie-cowsay-args  "-f tux -s")
    (fortune-cookie-mode)
    )
  )

(defun merlin-config/init-modern-fringes ()
  (use-package modern-fringes
    :config
    (modern-fringes-invert-arrows)
    )
  )
