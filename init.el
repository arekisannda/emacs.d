;; init.el --- Emacs Configuration Init -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)

(defun init/recursive-load-path (path)
  "Recursively load subdirectories in PATH."
  (let* ((path (expand-file-name path user-emacs-directory))
         (local-pkgs (mapcar 'file-name-directory
                             (directory-files-recursively path"\\.el$"))))
    (if (file-accessible-directory-p path)
        (mapc (apply-partially 'add-to-list 'load-path) local-pkgs))))

(dolist (path '("lisp"))
  (init/recursive-load-path path))

(require 'packages-manager)
(require 'packages-base)
(require 'packages-treesit)
(require 'packages-hs-mode)
(require 'packages-themes)
(require 'packages-interface)
(require 'packages-evil)
(require 'packages-vc)
(require 'packages-vterm)
(require 'packages-lsp-mode)
(require 'packages-dap-mode)
(require 'packages-flycheck)
(require 'packages-perspective)
(require 'packages-treemacs)
(require 'packages-project)
(require 'packages-windows)
(require 'packages-layout)
(require 'packages-languages)
(require 'packages-japanese)
(require 'packages-latex)
(require 'packages-org-mode)
(require 'packages-yasnippet)
(require 'packages-corfu)
(require 'packages-vertico)
(require 'packages-ispell)
(require 'packages-emacs)

(provide 'config/init)

;;; init.el ends here
