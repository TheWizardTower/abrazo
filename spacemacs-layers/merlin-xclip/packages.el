(defconst merlin-xclip-packages '(xclip))

(defun merlin-xclip/init-xclip ()
  (use-package xclip
    :config
    (xclip-mode 1)
    )
  )
