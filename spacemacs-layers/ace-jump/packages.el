(defconst ace-jump-packages
  '(
  ace-isearch
  ace-jump-buffer
  ace-popup-menu
  ))

(defun ace-jump/init-ace-window ()
     (use-package ace-window)
     (global-unset-key (kbd "C-x o"))
     (global-set-key (kbd "C-x o") 'ace-window))


(defun ace-jump/init-ace-isearch ()
     (use-package ace-isearch)
     (global-ace-isearch-mode +1))

(defun ace-jump/init-ace-jump-buffer ()
     (use-package ace-jump-buffer))

(defun ace-jump/init-ace-popup-menu ()
     (use-package ace-popup-menu)
     (ace-popup-menu-mode 1))

