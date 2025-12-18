;;; lang/go.el -*- lexical-binding: t; -*-

(require 'util-lang)

(defun +lang-go-flymake-setup ()
  "Setup to run for `flymake-golanci`."
  (setq-local flymake-golangci-executable (executable-find "golangci-lint"))
  (flymake-golangci-load))

(defun +lang-go-mode-setup ()
  "Setup to run for `go` modes."
  (add-hook 'before-save-hook #'gofmt-before-save nil 'local)
  (when (featurep 'envrc)
    (add-hook '+envrc-update-hook #'+lang-go-flymake-setup nil t)))

(use-package flymake-golangci :defer t)

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

(use-package ob-go :defer t)
