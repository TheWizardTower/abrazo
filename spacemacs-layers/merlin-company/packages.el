(defconst merlin-company-packages '(company-dict company-fuzzy company-irony company-irony-c-headers company-lsp company-math company-plsense company-restclient))

(defun merlin-company/init-company-dict ()
  (use-package company-dict)
  )

(defun merlin-company/init-company-fuzzy ()
  (use-package company-fuzzy)
  )

(defun merlin-company/init-company-irony ()
  (use-package company-irony)
  )

(defun merlin-company/init-company-irony-c-headers ()
  (use-package company-irony-c-headers)
  )

(defun merlin-company/init-company-lsp ()
  (use-package company-lsp)
  )

(defun merlin-company/init-company-math ()
  (use-package company-math)
  )

(defun merlin-company/init-company-plsense ()
  (use-package company-plsense)
  )

;; (defun merlin-company/init-company-quickhelp ()
;;   (use-package company-quickhelp
;;     :bind
;;     (:map company-active-help ("M-h" . company-quickhelp-manual-begin)
;;           )
;;     )
;;   )

(defun merlin-company/init-company-restclient ()
  (use-package company-restclient
    :config
    (push 'company-restclient company-backends)
    )
  )
