(defconst merlin-config-packages '(cloc doom-themes fortune-cookie modern-fringes))

(defun merlin-config/init-cloc ()
  (use-package cloc)
  )

(defun merlin-config/init-doom-themes ()
  (use-package doom-themes
    :config
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled
    (doom-themes-visual-bell-config)
    (doom-themes-org-config)
    )
  )

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
