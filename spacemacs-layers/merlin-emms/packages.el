(defconst merlin-emms-packages '(emms helm-emms))

(defun merlin-emms/init-emms ()
  (use-package emms
    :config
    (progn
      (require 'emms-player-simple)
      (require 'emms-source-file)
      (require 'emms-source-playlist)
      (require 'emms-player-mplayer)
      (setq emms-player-list '(emms-player-mplayer))
      (setq emms-directory "~/Music/")
      (setq emms-source-file-default-directory "~/Music/")
      (emms-add-directory-tree "~/Music/")
      )
    )
  )

(defun merlin-emms/init-helm-emms ()
  (use-package helm-emms)
  )
