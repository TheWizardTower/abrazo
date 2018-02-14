
;; ghcid isn't on melpa, so we have to do things the ghetto way. :(
(load (expand-file-name "~/git/ghcid/plugins/emacs/ghcid"))
(add-hook 'haskell-mode 'ghcid)
