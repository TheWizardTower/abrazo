(defconst merlin-terraform-packages '(terraform-doc))

(defun merlin-terraform/init-terraform-doc ()
  (use-package terraform-doc
    :defer t
    :init
    ;; This block executes before the package has been loaded
    :config
    ;; This block executes after the package has been loaded
    ))
