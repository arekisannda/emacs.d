;; init.el --- Emacs Configuration Init -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defun init/recursive-load-path (path)
  "Recursively load subdirectories in PATH."
  (let* ((path (expand-file-name path user-emacs-directory))
         (local-pkgs (mapcar 'file-name-directory
                             (directory-files-recursively path"\\.el$"))))
    (if (file-accessible-directory-p path)
        (mapc (apply-partially 'add-to-list 'load-path) local-pkgs))))

(dolist (path '("config"
                "util"))
  (init/recursive-load-path path))

(require 'packages-init)

(elpaca-wait)

(require 'tools-treesit)
(require 'tools-ispell)
(require 'tools-org)
(require 'tools-diff)

(require 'management-init)
(require 'ui-init)
(require 'editor-init)
(require 'keybinds-init)

(provide 'config/init)

;;; init.el ends here
