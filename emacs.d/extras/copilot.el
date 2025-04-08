;;; package -- summary
;;; Code to load Copilot fanciness into the world's greatest operating system.
;;; Now it can talk to me. But it's still missing a text editor. Such
;;; is the absurdity of life.

;;; Commentary:
;;; reference:
;;; https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup for
;;; further config options.

;;; Code
(use-package editorconfig)
(use-package copilot-chat)
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
	    :rev :newest
	    :branch "main")
  :hook
  (('prog-hook-mode . 'copilot-mode))
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  )
