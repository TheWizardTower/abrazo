(defconst merlin-moody-packages '(moody))

(defun merlin-moody/init-moody ()
  (use-package moody
    :config
    (setq x-underline-at-descent-line t)
    (moody-replace-mode-line-buffer-identification)
    (moody-replace-vc-mode)
    )
  )
