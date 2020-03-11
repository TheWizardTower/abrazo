(defconst merlin-dotnet-packages '(csproj-mode dotnet))

(defun merlin-dotnet/init-csproj-mode ()
  (use-package csproj-mode)
  )

(defun merlin-dotnet/init-dotnet ()
  (use-package dotnet
    :config
    (add-hook 'csharp-mode-hook 'dotnet-mode)
    (add-hook 'fsharp-mode-hook 'dotnet-mode)
    )
  )
