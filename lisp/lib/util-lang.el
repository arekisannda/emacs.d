;;; util-lang.el --- Emacs Language Configuration Utlity Functions  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'util-helpers)

(defun util/lang--set-auto-mode (mode-alist)
  "Update `auto-mode-alist` with MODE-ALIST."
  (cl-loop for mode in mode-alist do
           (util/dedup-add-to-list 'auto-mode-alist mode)))

(defun util/lang--add-to-capf-list (capf-list)
  "Update `completion-at-point-functions` with CAPF-LIST."
  (dolist (capf capf-list)
    (add-to-list 'completion-at-point-functions capf)))

(defcustom util/lsp-default-disabled-modes '(org-mode
                                            emacs-lisp-mode)
  "List of LSP-disabled modes by default."
  :type '(repeat symbol)
  :group 'eglot
  :group 'convenience)

(defun util/lsp-ensure ()
  "Helper function to enable LSP."
  (interactive)
  (unless (bound-and-true-p +lsp-disable)
    (eglot-ensure)))

(defun util/lsp-ensure-modes ()
  "Helper wrapper function to enable LSP."
  (unless (derived-mode-p util/lsp-default-disabled-modes)
    (util/lsp-ensure)))

(provide 'util-lang)

;;; util-lang.el ends here
