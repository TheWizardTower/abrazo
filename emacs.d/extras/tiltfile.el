;;; Tilt / Starlark development configuration

;; Mirrors the neovim setup:
;;   - nvim/ftdetect/tiltfile.vim maps Tiltfile / *Tiltfile* -> bzl filetype
;;   - nvim/lsp/starlark_rust_alt.lua runs `starlark --lsp` for bzl/Tiltfile
;;   - nvim/lsp/starlark.lua runs `tilt lsp start` (opt-in, gated on binary)

(use-package bazel
  :mode (("\\.bzl\\'"                              . bazel-starlark-mode)
         ("\\.star\\'"                             . bazel-starlark-mode)
         ("Tiltfile"                               . bazel-starlark-mode)
         ("\\(^\\|/\\)BUILD\\(\\.bazel\\)?\\'"     . bazel-build-mode)
         ("\\(^\\|/\\)WORKSPACE\\(\\.bazel\\)?\\'" . bazel-workspace-mode)
         ("\\(^\\|/\\)MODULE\\.bazel\\'"           . bazel-module-mode)))

(with-eval-after-load 'lsp-mode
  ;; starlark-rust: `cargo install starlark` — good for linting/diagnostics
  (when (executable-find "starlark")
    (add-to-list 'lsp-language-id-configuration
                 '(bazel-starlark-mode . "starlark"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection '("starlark" "--lsp"))
      :major-modes '(bazel-starlark-mode bazel-build-mode
                     bazel-workspace-mode bazel-module-mode)
      :priority -1
      :server-id 'starlark-rust))
    (add-hook 'bazel-starlark-mode-hook  #'lsp-deferred)
    (add-hook 'bazel-build-mode-hook     #'lsp-deferred)
    (add-hook 'bazel-workspace-mode-hook #'lsp-deferred)
    (add-hook 'bazel-module-mode-hook    #'lsp-deferred))

  ;; Tilt's own LSP (`tilt lsp start`) — richer for Tiltfile-specific APIs.
  ;; Registered with higher priority; takes precedence when both are present.
  (when (executable-find "tilt")
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection '("tilt" "lsp" "start"))
      :major-modes '(bazel-starlark-mode)
      :priority 1
      :server-id 'tilt-lsp))))

(provide 'tiltfile)
;;; tiltfile.el ends here
