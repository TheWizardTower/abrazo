(defconst merlin-yasnippet-packages '(code-library common-lisp-snippets el-autoyas go-snippets java-snippets org-sync-snippets yatemplate))

(defun merlin-yasnippet/init-code-library ()
  (use-package code-library
    :config
    (setq code-library-directory "~/abrazo/code-library/")
    )
  )

(defun merlin-yasnippet/init-common-lisp-snippets ()
  (use-package common-lisp-snippets)
  )

(defun merlin-yasnippet/init-el-autoyas ()
  (use-package el-autoyas
    :config
    )
  )

(defun merlin-yasnippet/init-go-snippets ()
  (use-package go-snippets)
  )

(defun merlin-yasnippet/init-java-snippets ()
  (use-package java-snippets)
  )

(defun merlin-yasnippet/init-org-sync-snippets ()
  (use-package org-sync-snippets
    :config
    (setq org-sync-snippets-snippets-dir "~/abrazo/org-sync-snippets")
    (setq org-sync-snippets-org-snippets-file "~/abrazo/org-sync-snippets/snippets.org")
    (add-hook 'yas-after-reload-hook #'org-sync-snippets-snippets-to-org)
    )
  )

(defun merlin-yasnippet/init-yatemplate ()
  (use-package yatemplate)
  )
