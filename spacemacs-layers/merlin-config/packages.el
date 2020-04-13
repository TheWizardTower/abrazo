(defconst merlin-config-packages '(free-keys fortune-cookie))

(defun merlin-config/init-free-keys ()
  (use-package free-keys))

(defun merlin-config/init-fortune-cookie ()
  (use-package fortune-cookie
    :config
    (setq fortune-cookie-cowsay-args  "-f tux -s")
    (fortune-cookie-mode)
    )
  )
