;;; package -- summary
;;; Code to load Copilot fanciness into the world's greatest operating system.
;;; Now it can talk to me. But it's still missing a text editor. Such
;;; is the absurdity of life.

;;; Commentary:
;;; reference:
;;; https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup for
;;; further config options.

;;; Code:
(use-package editorconfig)

(use-package copilot-chat)

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :hook (prog-mode . copilot-mode)
  :config
  ;; Keybindings must be set in :config after the package loads
  ;; because copilot-completion-map doesn't exist until then
  (define-key copilot-completion-keymap (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-keymap (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-completion-keymap (kbd "C-TAB") 'copilot-accept-completion-by-word)
  (define-key copilot-completion-keymap (kbd "C-<tab>") 'copilot-accept-completion-by-word))

(provide 'copilot)
;;; copilot.el ends here



;; ;;; package -- summary
;; ;;; Code to load Copilot fanciness into the world's greatest operating system.
;; ;;; Now it can talk to me. But it's still missing a text editor. Such
;; ;;; is the absurdity of life.

;; ;;; Commentary:
;; ;;; reference:
;; ;;; https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup for
;; ;;; further config options.

;; ;;; Code:
;; (use-package editorconfig)

;; (use-package copilot-chat)

;; (use-package copilot
;;   :vc (:url "https://github.com/copilot-emacs/copilot.el"
;;             :rev :newest
;;             :branch "main")
;;   :hook (prog-mode . copilot-mode)
;;   :bind (:map copilot-completion-keymap
;;               ("<tab>" . copilot-accept-completion)
;;               ("TAB" . copilot-accept-completion)
;;               )
;;   )

;; (provide 'copilot)
;; ;;; copilot.el ends here
