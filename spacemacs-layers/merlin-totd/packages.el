(defconst merlin-totd-packages '(totd))

(defun merlin-totd/init-totd ()
  (use-package totd
    :config
    (totd-start)
    (spacemacs/set-leader-keys "h'" 'totd)
    )
  )
