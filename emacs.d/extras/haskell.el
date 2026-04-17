(use-package haskell-mode)
(use-package lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(use-package flycheck-haskell
  :hook (flycheck-mode . flycheck-haskell-setup))
