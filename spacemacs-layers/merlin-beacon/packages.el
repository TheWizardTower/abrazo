
(defconst merlin-beacon-packages '(beacon))

(defun merlin-beacon/init-beacon ()
  (use-package beacon
    :config
    (beacon-mode 1))
  )
