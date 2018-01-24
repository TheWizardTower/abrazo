
(defconst merlin-highlight-thing-packages '(highlight-thing))

(defun merlin-highlight-thing/init-highlight-thing ()
  (use-package highlight-thing
    :config
    (global-highlight-thing-mode)
    )
  )
