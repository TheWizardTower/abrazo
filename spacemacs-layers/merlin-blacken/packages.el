(defconst merlin-blacken-packages '(blacken))

(defun merlin-blacken/init-blacken ()
  (use-package blacken
    :config
    (add-hook 'python-mode-hook 'blacken-mode)
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "F" 'blacken-buffer)
    )
  )
