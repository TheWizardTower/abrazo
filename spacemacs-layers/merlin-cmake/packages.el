(defconst merlin-cmake-packages '(cmake-font-lock cmake-project cpputils-cmake eldoc-cmake))

(defun merlin-cmake/init-cmake-font-lock ()
  (use-package cmake-font-lock)
  )

(defun merlin-cmake/init-cmake-project ()
  (use-package cmake-project)
  )

(defun merlin-cmake/init-cpputils-cmake ()
  (use-package cpputils-cmake
    :config
    (add-hook 'c-mode-common-hook
              (lambda ()
                (if (derived-mode-p 'c-mode 'c++-mode)
                    (cppcm-reload-all)
                  )))
    ;; OPTIONAL, avoid typing full path when starting gdb
    (global-set-key (kbd "C-c C-g")
                    '(lambda ()(interactive) (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer)))))
    ;; OPTIONAL, some users need specify extra flags forwarded to compiler
    (setq cppcm-extra-preprocss-flags-from-user '("-I/usr/src/linux/include" "-DNDEBUG")
          )
    )
  )

(defun merlin-cmake/init-eldoc-cmake ()
  (use-package eldoc-cmake
    :hook (cmake-mode . eldoc-cmake-enable)
    )
  )
