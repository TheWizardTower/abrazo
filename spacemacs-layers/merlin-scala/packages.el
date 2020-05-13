(defconst merlin-scala-packages '(company-posframe electric-operator helm-posframe posframe))

(defun merlin-scala/init-company-posframe ()
  (use-package company-posframe
    :after posframe
    )
  )

(defun merlin-scala/init-electric-operator ()
  (use-package electric-operator
    :delight
    :config
    ;; (add-hook 'prog-mode-hook 'electric-operator-mode)
    (add-hook 'scala-mode-hook 'electric-operator-mode)
    (apply #'electric-operator-add-rules-for-mode 'scala-mode
           (electric-operator-get-rules-for-mode 'prog-mode))
    (electric-operator-add-rules-for-mode 'scala-mode
                                          (cons "<-" " <- ")
                                          (cons "->" " -> ")
                                          (cons "=>" " => ")
                                          (cons "<:" " <: ")
                                          (cons ":>" " :> ")
                                          (cons "<%" " <% ") ;; deprecated
                                          (cons "%%" " %% ")
                                          (cons "%%%" " %%% ")
                                          (cons "/*" " /* ")
                                          (cons "//" " // ")
                                          (cons "++" " ++ ")
                                          (cons "++=" " ++= ")
                                          ;; Cats operators
                                          (cons "*>" " *> ")
                                          (cons "<*" " <* ")
                                          (cons "===" " === ")
                                          (cons "=!=" " =!= ")
                                          (cons ">>=" " >>= ")
                                          (cons ">>" " >> ")
                                          (cons "|-|" " |-| ")
                                          (cons "|+|" " |+| ")
                                          (cons "<+>" " <+> ")
                                          (cons "<+>" " <+> ")
                                          (cons "<<<" " <<< ")
                                          (cons ">>>" " >>> ")
                                          (cons "&&&" " &&& ")
                                          (cons "-<" " -< ")
                                          (cons "~>" " ~> ")
                                          (cons ":<:" " :<: ")
                                          (cons "&>" " &> ")
                                          (cons "<&" " <& ")
                                          (cons "<<" " << ")
                                          ;; sbt operators
                                          (cons ":=" " := ")
                                          )
    )
  )

(defun merlin-scala/init-helm-posframe ()
  (use-package helm-posframe
    :after posframe)
  )

(defun merlin-scala/init-posframe ()
  (use-package posframe)
  )
