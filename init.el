;; init.el --- Emacs Configuration Init -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; (profiler-start 'cpu+mem)

(use-package doom-themes
  :config
  (load-theme 'doom-monokai-pro t))

(elpaca-wait)

(require 'cl-lib)

;; load configuration files
(defvar +user-config-dir (expand-file-name "~/.config"))
(defvar +user-packages-dir (expand-file-name "lisp/packages" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el.gpg" user-emacs-directory))
(load custom-file 'noerror)
(defun +recursive-load-path (path)
  "Recursively load sub-directories in PATH."
  (if (file-regular-p path)
      (load path)
    (let* ((path (expand-file-name path user-emacs-directory))
           (local-pkgs (mapcar 'file-name-directory
                               (directory-files-recursively path"\\.el$"))))
      (if (file-accessible-directory-p path)
          (mapc (apply-partially #'add-to-list 'load-path) local-pkgs)))))

(dolist (path '("lisp"))
  (+recursive-load-path path))

;; enable configurations
(require 'packages-base)
(require 'packages-interface)
(require 'packages-utils)
(require 'packages-writing)
(require 'packages-windows)
(require 'packages-tools)
(require 'packages-completion)
(require 'packages-lsp)
(require 'packages-latex)
(require 'packages-org-mode)
(require 'packages-snippets)
(require 'packages-dashboard)
(require 'packages-code)
(require 'packages-dape)
(require 'packages-modes)
(require 'packages-emacs)

(provide 'config/init)

;;; init.el ends here
