;;; packages-emacs.el --- Emacs Built-in Packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package ediff
  :ensure nil
  :config
  ;; (defun ediff-setup-windows-custom (buffer-A buffer-B buffer-C control-buffer))
  (setq ediff-setup-windows-function 'ediff-setup-windows-merge))

(use-package emacs
  :after (general
          hydra
          evil
          vertico
          consult
          persp-mode
          corfu)
  :ensure nil
  :diminish auto-revert-mode
  :config
  (setq-default window-resize-pixelwise t)
  (setq-default frame-resize-pixelwise t)
  (setq-default hscroll-step 5)
  (setq-default scroll-step 5)
  (setq-default tab-width 4)
  (setq-default display-line-numbers-type 'relative)

  ;; performance tuning
  (defvar packages/emacs-gc-cons-threshold (* 1024 1024 100))
  (setq gc-cons-threshold packages/emacs-gc-cons-threshold)
  (setq read-process-output-max (* 1024 1024))

  (defvar packages/emacs--read-only-prefixes-list
    (list (expand-file-name elpaca-directory)
          (expand-file-name package-user-dir))
    "List of read-only file prefixes.")

  (defun packages/emacs--read-only-by-prefix ()
    "Enable `read-only-mode` if buffer includes one of `packages/emacs--read-only-prefixes-list`."
    (when (and buffer-file-name
               (cl-loop for prefix in packages/emacs--read-only-prefixes-list
                        thereis (string-prefix-p prefix buffer-file-name)))
      (read-only-mode 1)))

  (defun packages/emacs--visual-line-mode-setup ()
    "Setup to run for non `prog-mode` major modes."
    (setq truncate-lines nil)
    (visual-line-mode 1))

  (defun packages/emacs--exec-on-save ()
    "Operations to be executed on buffer save."
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max)))

  (add-hook 'before-save-hook #'packages/emacs--exec-on-save)
  (add-hook 'find-file-hook #'packages/emacs--read-only-by-prefix)
  (add-hook 'help-mode-hook #'packages/emacs--visual-line-mode-setup)
  (add-hook 'elpaca-after-init-hook #'(lambda () (load custom-file 'noerror)))

  (defun packages/emacs--minibuffer-setup ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun packages/emacs--minibuffer-exit ()
    (setq gc-cons-threshold packages/emacs-gc-cons-threshold))

  (add-hook 'minibuffer-setup-hook #'packages/emacs--minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'packages/emacs--minibuffer-exit)

  (require 'lang-generic)
  (require 'lang-elisp)
  (require 'lang-org)
  (require 'lang-clang)
  (require 'lang-csharp)
  (require 'lang-go)
  (require 'lang-lua)
  (require 'lang-python)

  (require 'keybinds-global)
  (require 'keybinds-evil)
  (require 'keybinds-session)
  (require 'keybinds-editor)
  (require 'keybinds-search)
  (require 'keybinds-completion))

(provide 'packages-emacs)

;;; packages-emacs.el ends here
