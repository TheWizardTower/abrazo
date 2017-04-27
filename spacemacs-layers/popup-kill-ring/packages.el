(defconst popup-kill-ring-packages
  '(popup-kill-ring)
  )

(defun popup-kill-ring/init-popup-kill-ring ()
  (use-package popup-kill-ring)
  (global-set-key "\M-y" 'popup-kill-ring))
