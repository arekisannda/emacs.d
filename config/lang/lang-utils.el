;;; lang-utils.el --- Emacs Language Configuration Utlity Functions  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'util-helpers)

(defun lang/utils--remap-major-mode (overwrite-alist)
  "Update `major-mode-remap-alist` with OVERWRITE-ALIST."
  (cl-loop for (mode . rmode) in overwrite-alist do
           (setq major-mode-remap-alist
                 (util/update-alist major-mode-remap-alist mode rmode))))

(defun lang/utils--set-auto-mode (mode-alist)
  "Update `auto-mode-alist` with MODE-ALIST."
  (cl-loop for mode in mode-alist do
           (util/dedup-add-to-list 'auto-mode-alist mode)))

(defun lang/utils--lang-config-filename ()
  "Return then name the current lang configuration file."
  (if-let ((file-name (buffer-file-name)))
      (let ((base-name (file-name-nondirectory file-name)))
        (file-name-sans-extension base-name))))

(defun lang/utils--add-to-capf-list (capf-list)
  "Update `completion-at-point-functions` with CAPF-LIST."
  (dolist (capf capf-list)
    (add-to-list 'completion-at-point-functions capf)))

(provide 'lang-utils)

;;; lang-utils.el ends here
