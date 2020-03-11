(defconst merlin-dotnet-packages '(dotnet))

(defun merlin-dotnet/init-dotnet ()
  (use-package dotnet
    :config
    (add-hook 'csharp-mode-hook 'dotnet-mode)
    (add-hook 'fsharp-mode-hook 'dotnet-mode)
    )
  )

