
(defconst merlin-evil-goggles-packages '(evil-goggles))

(defun merlin-evil-goggles/init-evil-goggles ()
  (use-package evil-goggles)

  :ensure t
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces)
  (setq evil-goggles-pulse 't)
  (setq evil-goggles-duration 0.500) ;; default is 0.200
  (setq evil-ace-jump-active 't)
  )
