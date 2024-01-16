;;; permissions.el --- Emacs permissions configurations -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defvar configs--read-only-prefixes-list (list (expand-file-name elpaca-directory)
                                               (expand-file-name package-user-dir))
  "List of read-only file prefixes.")

(defun configs--set-read-only-mode ()
  "Enable read-only mode for files."
  (when (and buffer-file-name
             (or (cl-loop for prefix in configs--read-only-prefixes-list
                          thereis (string-prefix-p prefix buffer-file-name))))
    (read-only-mode 1)))

(add-hook 'find-file-hook #'configs--set-read-only-mode)

(provide 'configs-permissions)

;;; permissions.el ends here
