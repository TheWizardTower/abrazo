;;; Shamelessly stolen from https://github.com/Xe/dotfiles/blob/master/spacemacs/xe-matrix/packages.el

(setq xe-matrix-packages
      '(matrix-client))

(defun xe-matrix/init-matrix-client ()
  (use-package matrix-client))
