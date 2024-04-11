;;; util-lang.el --- Emacs Language Configuration Utlity Functions  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'util-helpers)

(defun util/lang--remap-major-mode (overwrite-alist)
  "Update `major-mode-remap-alist` with OVERWRITE-ALIST."
  (cl-loop for (mode . rmode) in overwrite-alist do
           (setq major-mode-remap-alist
                 (util/update-alist major-mode-remap-alist mode rmode))))

(defun util/lang--set-auto-mode (mode-alist)
  "Update `auto-mode-alist` with MODE-ALIST."
  (cl-loop for mode in mode-alist do
           (util/dedup-add-to-list 'auto-mode-alist mode)))

(defun util/lang--add-to-capf-list (capf-list)
  "Update `completion-at-point-functions` with CAPF-LIST."
  (dolist (capf capf-list)
    (add-to-list 'completion-at-point-functions capf)))

(defun util/lsp-ensure ()
  "Helper function to enable LSP."
  (unless (bound-and-true-p +lsp-disable)
    (eglot-ensure)))

(provide 'util-lang)

;;; util-lang.el ends here
