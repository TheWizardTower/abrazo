
(defconst merlin-evil-easymotion-packages '(evil-easymotion))

(defun merlin-evil-easymotion/init-evil-easymotion ()
  (use-package evil-easymotion)
  (evilem-default-keybindings "<backtab>")
  )
