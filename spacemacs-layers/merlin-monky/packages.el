(defconst merlin-monky-packages
  '(monky))

(defun merlin-monky/init-monky ()
                              (use-package monky)
                              (setq monky-process-type 'cmdserver))
