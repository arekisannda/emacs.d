;;; util-lang.el --- Emacs Language Configuration Utlity Functions  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'util-helpers)

(defun util/lang--set-auto-mode (mode-alist)
  "Update `auto-mode-alist` with MODE-ALIST."
  (cl-loop for mode in mode-alist do
           (util/dedup-add-to-list 'auto-mode-alist mode)))

(defun util/add-capf-hooks (&rest capf-list)
  "Add CAPF-LIST hooks from `completion-at-point-functions`."
  (dolist (capf capf-list)
    (add-hook 'completion-at-point-functions capf nil t)))

(defun util/remove-capf-hooks (&rest capf-list)
  "Remove CAPF-LIST hooks from `completion-at-point-functions`."
  (dolist (capf capf-list)
    (remove-hook 'completion-at-point-functions capf t)))

(defcustom util/lsp-default-disabled-modes '(org-mode
                                             emacs-lisp-mode)
  "List of LSP-disabled modes by default."
  :type '(repeat symbol)
  :group 'eglot
  :group 'convenience)

(defvar-local util/lsp-disabled nil)

(defun util/lsp-ensure ()
  "Helper function to enable LSP."
  (interactive)
  (unless (bound-and-true-p util/lsp-disabled)
    (eglot-ensure)))

(defun util/lsp-ensure-modes ()
  "Helper wrapper function to enable LSP."
  (unless (derived-mode-p util/lsp-default-disabled-modes)
    (add-hook 'hack-local-variables-hook #'util/lsp-ensure t t)
    t))

(provide 'util-lang)

;;; util-lang.el ends here
