(defconst magithub-packages '(magithub))


(defun magithub/init-magithub ()
  (use-package magithub
    :after magit
    :config (magithub-feature-autoinject t)))
