;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.layers/")
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ace-jump
     ag
     ansible
     (auto-completion
      :variables
      auto-completion-enable-help-tooltip t
      auto-completion-return-key-behavior 'complete
      auto-completion-tab-key-behavior 'cycle
      auto-completion-complete-with-key-sequence "jk"
      auto-completion-enable-snippets-in-popup t
      auto-completion-enable-help-tooltip t
      auto-completion-use-company-box t
      )
     (c-c++
      :variables
      c-c++-backend 'lsp-ccls
      c-c++enable-clang-support t)
     clojure
     cmake
     command-log
     csharp
     csv
     emacs-lisp
     evernote
     fsharp
     git
     go
     (haskell
      :variables
      haskell-completion-backend 'lsp
      haskell-enable-hindent-style 'fundamental
      haskell-indent-spaces 2
      haskell-process-args-ghci "ghci"
      haskell-process-type 'stack-ghci
      haskell-stylish-on-save 't
      )
     helm
     html
     idris
     javascript
     (latex :variables
            latex-enable-auto-fill t)
     lsp
     markdown
     merlin-base-convert
     merlin-beacon
     merlin-cmake
     merlin-company
     merlin-config
     merlin-dired
     merlin-dotnet
     merlin-double-saber
     merlin-elisp
     merlin-eshell
     merlin-evil
     merlin-flycheck
     merlin-format-all
     merlin-hacklang
     merlin-helm
     merlin-jq
     merlin-kubernetes
     merlin-magit
     merlin-markdownfmt
     merlin-monky
     merlin-python
     merlin-rpn
     merlin-scala
     merlin-shell
     merlin-terraform
     merlin-totd
     merlin-ts
     merlin-writegood
     merlin-yaml
     merlin-yasnippet
     merlin-xclip
     nixos
     (org
      :variables
      org-enable-reveal-js-support t
      org-enable-github-support t)
     pandoc
     php
     popup-kill-ring
     projectile-ripgrep
     purescript
     python
     rust
     semantic
     semantic-web
     scala
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     shell-scripts
     spacemacs-editing
     (spell-checking
      :variables
      spell-checking-enable-by-default t)
     sql
     (syntax-checking
      :variables
      syntax-checking-enable-tooltips t)
     terraform
     (typescript :variables
                 typescript-fmt-on-save t
                 typescript-fmt-tool 'typescript-formatter
                 typescript-indent-level 2
                 )
     version-control
     yaml
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(cl ensime)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         doom-palenight
                         doom-dark+
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Inconsolata for Powerline"
                               :size 15
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Abrazos"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize 't
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.2
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers 'relative
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq evil-want-keybinding nil)
  (setq lsp-keymap-prefix "C-c l")
  (if (getenv "TMUX")
      (progn
        (message "Tmux detected")
        (setq shell-default-term-shell "/usr/bin/bash")
        )
    (progn
      (message "No tmux!")
      (setq shell-default-term-shell "~/local/bin/bash_tmux.sh")
      )
    )
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (message "Running user-config")
  (add-to-list 'exec-path (expand-file-name "~/local/bin"))
  (add-to-list 'exec-path (expand-file-name "~/go/bin"))
  (add-to-list 'exec-path (expand-file-name "~/gocode/bin"))
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
  (global-set-key (kbd "C-x C-j") 'dired-jump)
  (yas-reload-all)
  (spacemacs/set-leader-keys-for-major-mode 'c++-mode "F" 'clang-format-buffer)
  (load (expand-file-name "~/git/ghcid/plugins/emacs/ghcid"))
  (add-hook 'haskell-mode-hook 'ghcid)
  (setq auto-mode-alist (cons '("\\.g\\'" . antlr-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.g4\\'" . antlr-mode) auto-mode-alist))
  (defun lsp-hook ()
    (message "Running lsp hook!")
    (lsp)
    )
  (defun sort-words-region (&optional b e)
    (interactive "r")
    (call-process-region b e "sort-words.sh" t t)
    )
  (add-hook 'c-mode-hook 'lsp-hook)
  (add-hook 'c++-mode-hook 'lsp-hook)
  (add-hook 'haskell-mode-hook 'lsp-hook)
  (add-hook 'purescript-mode-hook 'lsp-hook)
  (add-hook 'python-mode-hook 'lsp-hook)
  (add-hook 'rust-mode-hook 'lsp-hook)
  (add-hook 'scala-mode-hook 'lsp-hook)
  (add-hook 'typescript-mode-hook 'lsp-hook)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yatemplate xclip writegood-mode tss yaxception ts-comint travis totd shfmt rpn-calc python-x folding python-test python-pytest python-info python-environment deferred python-docstring python-black pyimpsort pyimport shut-up projectile-ripgrep ripgrep powerline-evil popup-kill-ring org-sync-snippets monky modern-fringes mo-git-blame markdownfmt magit-todos magit-org-todos magit-find-file magit-filenotify magit-circleci kubernetes-tramp kubernetes-helm kubernetes-evil kubernetes k8s-mode jq-mode jq-format reformatter java-snippets helm-proc helm-posframe helm-lastpass csv helm-ispell helm-helm-commands helm-git-files helm-flyspell helm-flycheck helm-file-preview helm-dired-history helm-describe-modes helm-cmd-t hack-mode go-snippets fortune-cookie format-all language-id flycheck-yamllint flycheck-pycheckers flycheck-popup-tip flycheck-mypy flycheck-indicator flycheck-clangcheck flycheck-clang-tidy flycheck-clang-analyzer flycheck-checkbashisms fd-dired evil-space evil-smartparens evil-owl evil-mark-replace evil-fringe-mark evil-extra-operator evil-expat evil-ex-shell-command evil-ex-fasd fasd evil-embrace embrace evil-easymotion evil-collection annalist evil-avy eshell-git-prompt eshell-fringe-status eshell-did-you-mean elsa trinary elpl electric-operator eldoc-cmake el-autoyas double-saber dotnet doom-themes diredful dired-toggle-sudo dired-rsync dired-rifle dired-recent dired-ranger dired-rainbow dired-quick-sort dired-narrow dired-hacks-utils dired-launch dired-k dired-git-info dired-git ppp all-the-icons async-await iter2 promise memoize leaf csproj-mode cpputils-cmake company-restclient restclient know-your-http-well company-posframe posframe company-plsense company-math math-symbol-lists company-lsp company-irony-c-headers company-irony irony company-fuzzy company-dict common-lisp-snippets code-library gist gh marshal logito pcache cmake-project cmake-font-lock cloc beacon bash-completion artbollocks-mode ag ace-popup-menu avy-menu ace-jump-zap ace-jump-mode ace-jump-buffer ace-isearch 0xc yapfify yaml-mode xterm-color web-mode web-beautify toml-mode tide typescript-mode tern tagedit stickyfunc-enhance srefactor sql-indent powerline smeargle slim-mode shell-pop scss-mode sass-mode racer pyvenv pytest pyenv-mode py-isort pug-mode psci purescript-mode psc-ide pip-requirements phpunit phpcbf php-extras php-auto-yasnippets pandoc-mode ox-reveal ox-pandoc ox-gfm orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download omnisharp multi-term mmm-mode markdown-toc magit-gitflow magit-popup livid-mode skewer-mode simple-httpd live-py-mode json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc jinja2-mode intero insert-shebang idris-mode prop-menu hy-mode htmlize hlint-refactor hindent parent-mode helm-pydoc projectile helm-hoogle helm-gitignore helm-css-scss helm-company helm-c-yasnippet haskell-snippets haml-mode go-guru go-eldoc gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md geeknote fuzzy fsharp-mode eglot project flymake eldoc xref flyspell-correct-helm flyspell-correct flycheck-rust flycheck-pos-tip flycheck-haskell flycheck flx fish-mode highlight evil-magit magit git-commit with-editor transient smartparens iedit anzu evil goto-chg undo-tree eshell-z eshell-prompt-extras esh-help emmet-mode drupal-mode php-mode disaster diff-hl cython-mode csv-mode csharp-mode company-web web-completion-data company-statistics company-shell company-quickhelp pos-tip company-go go-mode company-ghci company-ghc ghc haskell-mode company-cabal company-c-headers company-auctex company-ansible company-anaconda company command-log-mode coffee-mode cmm-mode cmake-mode clojure-snippets clj-refactor hydra inflections multiple-cursors paredit lv clang-format cider-eval-sexp-fu cider sesman spinner queue pkg-info parseedn clojure-mode parseclj a epl cargo rust-mode bind-map bind-key auto-yasnippet yasnippet auto-dictionary packed auctex ansible-doc ansible anaconda-mode pythonic f dash s helm avy helm-core async ac-ispell auto-complete popup scala-mode sbt-mode noflet lsp-ui lsp-treemacs treemacs pfuture helm-lsp lsp-mode markdown-mode ht dash-functional ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump diminish define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   (quote
    (csv-mode scala-mode sbt-mode noflet lsp-ui lsp-treemacs treemacs pfuture helm-lsp lsp-mode markdown-mode ht dash-functional ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump diminish define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line)))
 '(psc-ide-add-import-on-completion t t)
 '(psc-ide-rebuild-on-save nil t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
