;;; packages-code.el --- Coding Package Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'util-folding)
(require 'util-lang)
(require 'mule-util)

(setq +fold-replacement (concat " " (truncate-string-ellipsis) " "))

(defcustom elisp-initial-hide-level '()
  "Set initial hide level ."
  :type '(repeat (cons (choice (const t) directory file)
                       (choice (const nil) (integer :tag "Level"))))
  :group 'hideshow)

(defun elisp-hide-check-level (buffer-name entry)
  (let* ((pred (car entry))
         (level (cdr entry)))
    (cond
     ((and (booleanp pred) pred) t)
     ((string-empty-p buffer-name) nil)
     ((and (directory-name-p pred) (file-in-directory-p buffer-name pred)) t)
     ((and (string= pred (file-name-nondirectory buffer-name))) t)
     )))

(defun elisp-hide-level ()
  (let* ((fn (apply-partially #'elisp-hide-check-level (or (buffer-file-name) "")))
         (level (cdr (cl-find-if fn elisp-initial-hide-level))))
    (save-excursion
      (goto-char (point-min))
      (when level (hs-hide-level level)))))

(use-package treesit
  :custom
  (treesit-font-lock-level 4)
  :config
  (setq treesit-language-source-alist
        '((bash            . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3"))
          (c               . ("https://github.com/tree-sitter/tree-sitter-c"))
          (c-sharp         . ("https://github.com/tree-sitter/tree-sitter-c-sharp"))
          (cmake           . ("https://github.com/uyha/tree-sitter-cmake"))
          (cpp             . ("https://github.com/tree-sitter/tree-sitter-cpp" "v0.21.0"))
          (css             . ("https://github.com/tree-sitter/tree-sitter-css"))
          (dockerfile      . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (elisp           . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (go              . ("https://github.com/tree-sitter/tree-sitter-go" "v0.23.4"))
          (gomod           . ("https://github.com/camdencheek/tree-sitter-go-mod" "v1.0.2"))
          (html            . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript      . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json            . ("https://github.com/tree-sitter/tree-sitter-json"))
          (kotlin          . ("https://github.com/fwcd/tree-sitter-kotlin"))
          (latex           . ("https://github.com/latex-lsp/tree-sitter-latex" "v0.3.0" "src"))
          (lua             . ("https://github.com/tree-sitter-grammars/tree-sitter-lua" "v0.4.0"))
          (make            . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown        . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.3.2" "tree-sitter-markdown/src"))
          (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.3.2" "tree-sitter-markdown-inline/src"))
          (nix             . ("https://github.com/nix-community/tree-sitter-nix"))
          (python          . ("https://github.com/tree-sitter/tree-sitter-python"))
          (rust            . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (toml            . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (tsx             . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
          (typescript      . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
          (typst           . ("https://github.com/uben0/tree-sitter-typst"))
          (yaml            . ("https://github.com/ikatyang/tree-sitter-yaml"))))

  (cl-loop for (lang) in treesit-language-source-alist do
           (unless (treesit-language-available-p lang)
             (treesit-install-language-grammar lang)
             (message "Treesit parser installed: %s" lang))))

(use-package treesit-fold :after treesit
  :custom
  (treesit-fold-replacement +fold-replacement)
  :custom-face
  (treesit-fold-replacement-face
   ((nil :inherit fold-replacement-face
         :box unspecified
         :weight unspecified
         :foreground unspecified
         :background unspecified
         )))
  :hook
  (treesit-fold-mode-on . (lambda () (setq-local util/fold-type 'treesit-fold
                                                 util/fold-show-all #'treesit-fold-open-all
                                                 util/fold-hide #'treesit-fold-close)))
  (treesit-fold-mode-off . (lambda () (setq-local util/fold-type nil
                                                  util/fold-show-all nil
                                                  util/fold-hide nil))))

(defun +hs-mode-fold-overlay (ov)
  "Format fold overlay OV."
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put
     ov
     'display
     (propertize +fold-replacement 'face 'treesit-fold-replacement-face))))

(use-package hideshow
  :custom
  (hs-set-up-overlay #'+hs-mode-fold-overlay)
  :hook
  (hs-minor-mode     . elisp-hide-level)
  (hs-minor-mode-on  . (lambda () (setq-local util/fold-type 'hs
                                              util/fold-show-all #'hs-show-all
                                              util/fold-hide #'hs-hide-block)))
  (hs-minor-mode-off . (lambda () (setq-local util/fold-type nil
                                              util/fold-show-all nil
                                              util/fold-hide nil))))

(use-package origami
  :custom
  (origami-fold-replacement +fold-replacement)
  :custom-face
  (origami-fold-replacement-face
   ((nil :inherit fold-replacement-face
         :box unspecified
         :weight unspecified
         :foreground unspecified
         :background unspecified
         ))))

(defun +lang-prog-mode-setup ()
  "Prog-mode setup."
  (setq-local origami-fold-style 'triple-braces)
  (setq-local truncate-lines t)

  (visual-line-mode -1)
  (completion-preview-mode 1)
  (diff-hl-mode 1)
  (display-line-numbers-mode 1)
  (rainbow-delimiters-mode 1)
  (origami-mode 1)
  (flyspell-prog-mode)
  (flymake-mode 1)
  (indent-bars-mode 1)
  (yas-minor-mode 1)

  (cond
   ((treesit-fold-ready-p) (treesit-fold-mode 1))
   ((derived-mode-p 'emacs-lisp-mode) (hs-minor-mode 1)))

  (util/add-capf-hooks
   #'cape-file
   #'cape-keyword)

  (origami-close-all-nodes (current-buffer)))

(use-package nil ;; prog-mode
  :hook
  (prog-mode . +lang-prog-mode-setup))

(defun +lang-rust-cargo-setup ()
  "Setup to run for `cargo` modes."
  (setq-local cargo-process--custom-path-to-bin (executable-find "cargo"))
  (setq-local cargo-process--rustc-cmd (executable-find "rustc")))

(defun +lang-rust-mode-setup ()
  "Setup to run for `rust` modes."
  (add-hook '+envrc-update-hook #'+lang-rust-cargo-setup))

(use-package cargo)

(use-package rust-mode
  :mode
  ("\\.rs\\'" . rust-ts-mode)
  :hook
  (rust-ts-mode . +lang-rust-mode-setup))

(use-package flymake-clippy
  :hook
  (rust-ts-mode . flymake-clippy-setup-backend))

(defun +lang-go-flymake-setup ()
  "Setup to run for `flymake-golanci`."
  (setq-local flymake-golangci-executable (executable-find "golangci-lint"))
  (flymake-golangci-load))

(defun +lang-go-mode-setup ()
  "Setup to run for `go` modes."
  (add-hook 'before-save-hook #'gofmt-before-save nil 'local)
  (when (envrc--find-env-dir)
    (add-hook '+envrc-update-hook #'+lang-go-flymake-setup)))

(use-package go-mode
  :custom
  (go-ts-mode-indent-offset 4)
  :mode
  ("\\.go\\'" . go-ts-mode)
  :hook
  (go-ts-mode . +lang-go-mode-setup))

(use-package flymake-golangci)

(defun +lang-clang-mode-setup ()
  "C or C++ mode setup."
  (setq c-ts-mode-indent-style 'k&r))

(use-package nil ;; c-ts-mode
  :hook
  (c-ts-base-mode . +lang-clang-mode-setup))

(use-package flymake-ruff)

(use-package nil ;; python-ts-mode
  :custom
  (python-indent-offset 4)
  :init
  (defun +lang-python-mode-setup ()
    (setq-local python-flymake-command '("flake8" "--max-line-length=120" "-"))
    (add-hook 'eglot-managed-mode-hook
              (lambda ()
                (add-hook 'flymake-diagnostic-functions #'python-flymake t t))
              nil t))
  :hook
  (python-ts-mode . +lang-python-mode-setup)
  (python-ts-mode . flymake-ruff-load))

(use-package typescript-mode
  :mode
  ("\\.tsx\\'" . tsx-ts-mode)
  ("\\.ts\\'" . typescript-ts-mode))

(defun +lang-lua-mode-setup ()
  "Lua-mode setup."
  (setq-local lua-indent-level 2)
  (setq-local lua-indent-nested-block-content-align nil)
  (setq-local lua-indent-close-paren-align nil)

  (advice-add
   #'lua-calculate-indentation-block-modifier
   :around #'(lambda (old-function &rest arguments)
               (let ((old-res (apply old-function arguments)))
                 (if (> old-res lua-indent-level) lua-indent-level old-res))))
  )

(use-package lua-mode
  :mode
  ("\\.lua\\'" . lua-ts-mode)
  :hook
  (lua-ts-mode . +lang-lua-mode-setup))

(use-package kotlin-mode)

(use-package kotlin-ts-mode
  :mode
  ("\\.kt\\'" . kotlin-ts-mode))

(use-package typst-ts-mode
  :custom
  (typst-ts-mode-indent-offset 2)
  (typst-ts-mode-watch-options "--open")
  (typst-ts-mode-enable-raw-blocks-highlight t)
  (typst-ts-mode-highlight-raw-blocks-at-startup t)
  :hook
  (typst-ts-mode . +lang-prog-mode-setup))

(use-package nil ;; sh-mode
  :custom
  (sh-basic-offset 2))

(defun +lang-elisp-exec-on-save ()
  "Operations to be executed on buffer save."
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max)))

(defun +lang-elisp-mode-setup ()
  "Setup to run for `emacs-lisp-mode` modes."
  (add-hook 'before-save-hook #'+lang-elisp-exec-on-save nil 'local)
  (util/add-capf-hooks
   #'cape-dabbrev
   #'cape-file
   #'cape-elisp-symbol
   #'cape-keyword))

(use-package elisp-mode
  :hook
  (emacs-lisp-mode . eldoc-mode)
  (emacs-lisp-mode . +lang-elisp-mode-setup))

(use-package scad-mode)

(use-package scad-dbus :after scad-mode)

(use-package plantuml-mode)

(use-package nix-ts-mode
  :mode
  ("\\.nix\\'" . nix-ts-mode))

(use-package nix-mode)

(defun +lang-conf-mode-setup ()
  "Conf-mode setup."
  (setq-local truncate-lines t)
  (visual-line-mode -1)
  (yas-minor-mode 1))

(use-package conf-mode
  :custom
  (json-ts-mode-indent-offset 2)
  :mode
  ("Dockerfile\\'" . dockerfile-ts-mode)
  ("\\.dockerfile\\'" . dockerfile-ts-mode)
  ("\\.jsonc\\'" . json-ts-mode)
  :hook
  (conf-mode . diff-hl-mode)
  (conf-mode . display-line-numbers-mode)
  (conf-mode . rainbow-delimiters-mode)
  (conf-mode . +lang-conf-mode-setup))

(use-package cmake-mode
  :mode
  ("CMakeLists\\.txt\\'" . cmake-mode))

(use-package flymake-json
  :hook
  (json-mode . flymake-json-load))

(use-package toml-ts-mode
  :mode
  ("\\.toml\\'" . toml-ts-mode)
  :hook
  (toml-ts-mode . diff-hl-mode)
  (toml-ts-mode . display-line-numbers-mode)
  (toml-ts-mode . rainbow-delimiters-mode)
  (toml-ts-mode . +lang-conf-mode-setup))

(use-package i3wm-config-mode
  :mode
  ("\\.sway\\'" . i3wm-config-mode)
  ("\\.i3\\'" . i3wm-config-mode)
  :hook
  (i3wm-config-mode . display-line-numbers-mode)
  (i3wm-config-mode . rainbow-delimiters-mode))

(use-package yaml-pro)

(use-package nil ;; yaml-ts-mode
  :custom
  (yaml-indent-offset 2)
  :mode
  ("\\.ya?ml\\'" . yaml-ts-mode)
  :hook
  (yaml-ts-mode . diff-hl-mode)
  (yaml-ts-mode . display-line-numbers-mode)
  (yaml-ts-mode . rainbow-delimiters-mode)
  (yaml-ts-mode . yaml-pro-ts-mode)
  (yaml-ts-mode . +lang-conf-mode-setup))

(use-package nil ;; css-mode
  :custom
  (css-indent-offset 2))

(use-package nil ;; xml-mode
  :mode
  ("\\.opf\\'" . xml-mode)
  ("\\.ncx\\'" . xml-mode))

(use-package markdown-mode
  :custom-face
  (markdown-code-face
   ((nil :background unspecified :inherit tooltip)))
  :config
  (util/update-alist
   'markdown-code-lang-modes
   '(
     ("go"         . go-mode)
     ("rust"       . rust-mode)
     ("diff"       . diff-mode)
     ("python"     . python-mode)
     ("javascript" . javascript-mode)
     ("typescript" . typescript-mode)
     ("kotlin"     . kotlin-mode)
     )))

(use-package tmux-mode)

(use-package mermaid-mode)

(use-package pdf-tools
  :custom
  (pdf-view-display-size 'fit-page)
  :mode
  ("\\.pdf\\'" . pdf-view-mode)
  :hook
  (pdf-view-mode . auto-revert-mode)
  (pdf-view-mode . pdf-view-midnight-minor-mode)
  (after-init    . pdf-loader-install))

(defun +lang-special-mode-setup ()
  "Setup to run for `special-mode` major modes."
  (cond
   ((string-match-p "\\*eldoc.*\\*" (buffer-name))
    (visual-line-mode 1))))

(use-package nil ; special-mode
  :hook
  (special-mode . +lang-special-mode-setup))

(use-package nil
  :custom
  (major-mode-remap-alist
   '((c++-mode        . c++-ts-mode)
     (c-mode          . c-ts-mode)
     (c-or-c++-mode   . c-or-c++-ts-mode)
     (csharp-mode     . csharp-ts-mode)
     (css-mode        . css-ts-mode)
     (go-dot-mod-mode . go-mod-ts-mode)
     (go-mode         . go-ts-mode)
     (javascript-mode . js-ts-mode)
     (js-json-mode    . json-ts-mode)
     (kotlin-mode     . kotlin-ts-mode)
     (lua-mode        . lua-ts-mode)
     (python-mode     . python-ts-mode)
     (sh-mode         . bash-ts-mode)
     (typescript-mode . typescript-ts-mode)
     )))

(provide 'packages-code)

;;; packages-code.el ends here
