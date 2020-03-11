(defconst merlin-kubernetes-packages '(kubernetes kubernetes-evil kubernetes-helm kubernetes-tramp k8s-mode))

(defun merlin-kubernetes/init-kubernetes ()
  (use-package kubernetes
    :ensure t
    :commands (kubernetes-overview)
    )
  )


(defun merlin-kubernetes/init-kubernetes-evil ()
  (use-package kubernetes-evil
    :ensure t
    :after kubernetes
    )
  )

(defun merlin-kubernetes/init-kubernetes-helm ()
  (use-package kubernetes-helm)
  )

(defun merlin-kubernetes/init-kubernetes-tramp ()
  (use-package kubernetes-tramp)
  )

(defun merlin-kubernetes/init-k8s-mode ()
  (use-package k8s-mode
    :ensure t
    :config
    (setq k8s-search-documentation-browser-function 'browse-url-firefox)
    :hook (k8s-mode . yas-minor-mode)
    )
  )
