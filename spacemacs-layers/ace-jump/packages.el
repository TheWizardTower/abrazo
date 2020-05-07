(defconst ace-jump-packages
  '(ace-isearch ace-jump-buffer ace-jump-mode ace-jump-zap ace-popup-menu)
  )

(defun ace-jump/init-ace-window ()
  (use-package ace-window
    :config
    (global-unset-key (kbd "C-x o"))
    (global-set-key (kbd "C-x o") 'ace-window)
    )
  )


(defun ace-jump/init-ace-isearch ()
  (use-package ace-isearch
    :config
    (global-ace-isearch-mode +1)
    )
  )

(defun ace-jump/init-ace-jump-buffer ()
  (use-package ace-jump-buffer
    :config
    (global-set-key (kbd "C-c b") 'ace-jump-buffer)
    )
  )
(defun ace-jump/init-ace-jump-mode ()
  (use-package ace-jump-mode
    :config
    (global-unset-key (kbd "C-c SPC"))
    (global-set-key (kbd "C-c SPC") 'ace-jump-mode)
    )
  )

(defun ace-jump/init-ace-jump-zap ()
  (use-package ace-jump-zap)
  )

(defun ace-jump/init-ace-popup-menu ()
  (use-package ace-popup-menu
    :config
    (ace-popup-menu-mode 1)
    )
  )
