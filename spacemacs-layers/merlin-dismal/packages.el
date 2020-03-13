(defconst merlin-dismal-packages '(dismal))

(defun merlin-dismal/init-dismal ()
  (use-package dismal
    :init
    (setq dismal-directory "~/org/")
    )
 )
