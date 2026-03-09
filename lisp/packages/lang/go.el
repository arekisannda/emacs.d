;;; lang/go.el -*- lexical-binding: t; -*-

(require 'util-lang)

(defun +lang-go-flymake-setup ()
  "Setup to run for `flymake-golanci`."
  (when-let ((exec (executable-find "golangci-lint")))
    (setq-local flymake-golangci-executable exec)
    (flymake-golangci-load)))

(defun +lang-go-mode-setup ()
  "Setup to run for `go` modes."
  (add-hook 'before-save-hook #'gofmt-before-save nil t)
  (when (+envrc-root)
    (add-hook '+envrc-update-hook #'+lang-go-flymake-setup nil t))
  (+lang-go-flymake-setup))

(use-package flymake-golangci)

(use-package go-mode
  :custom
  (go-ts-mode-indent-offset 4)
  :mode
  ("\\.go\\'" . go-ts-mode)
  :config
  (util/update-alist
   'major-mode-remap-alist
   '((go-dot-mod-mode . go-mod-ts-mode)
     (go-mode         . go-ts-mode)))
  :hook
  (go-ts-mode . +lang-go-mode-setup))

(use-package ob-go :defer t
  :config
  (setq org-babel-default-header-args:go '((:wrap . "example"))))
