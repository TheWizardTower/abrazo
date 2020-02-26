(defconst merlin-symon-packages
  '(symon)
  )

(defun merlin-symon/init-symon ()
  (use-package symon
    :config
    (symon-mode)
    )
  )
