(defconst merlin-config-packages '(fortune-cookie))

(defun merlin-config/init-fortune-cookie ()
  (use-package fortune-cookie
    :config
    (setq fortune-cookie-cowsay-args  "-f tux -s")
    (fortune-cookie-mode)
    )
  )
