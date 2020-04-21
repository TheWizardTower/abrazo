(defconst merlin-python-packages '(pyimport pyimpsort python-black python-docstring python-environment python-info python-pytest python-test python-x))

(defun merlin-python/init-pyimport ()
  (use-package pyimport
    :after python
    :config
    (define-key python-mode-map (kbd "C-c C-i") #'pyimport-insert-missing)
    )
  )

(defun merlin-python/init-pyimpsort ()
  (use-package pyimpsort
    :after python
    :config
    (define-key python-mode-map "\C-c\C-u" #'pyimpsort-buffer)
    )
  )

(defun merlin-python/init-python-black ()
  (use-package python-black
    :demand t
    :after python
    )
  )

(defun merlin-python/init-python-docstring ()
  (use-package python-docstring)
  )

(defun merlin-python/init-python-environment ()
  (use-package python-environment)
  )

(defun merlin-python/init-python-info ()
  (use-package python-info)
  )

(defun merlin-python/init-python-pytest ()
  (use-package python-pytest)
  )

(defun merlin-python/init-python-test ()
  (use-package python-test)
  )

(defun merlin-python/init-python-x ()
  (use-package python-x
    :config
    (setq python-section-delimiter "##")
    (python-x-setup)
    )
  )
