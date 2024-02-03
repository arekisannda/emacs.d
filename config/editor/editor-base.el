;;; editor-base.el --- Emacs Base Editor Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)

(require 'editor-input)

(defvar editor/base--read-only-prefixes-list
  (list (expand-file-name elpaca-directory)
        (expand-file-name package-user-dir))
  "List of read-only file prefixes.")

(defun editor/base--read-only-by-prefix ()
  "Enable `read-only-mode` if buffer includes one of `editor/base--read-only-prefixes-list`"
  (when (and buffer-file-name
             (or (cl-loop for prefix in editor/base--read-only-prefixes-list
                          thereis (string-prefix-p prefix buffer-file-name))))
    (read-only-mode 1)))

(defun editor/base--evil-mode-setup ()
  "Set up evil-mode packages."
  (evil-mode 1)
  (evil-collection-init)
  (evil-snipe-override-mode +1)
  (global-evil-matchit-mode 1)
  (evil-lion-mode 1)
  (global-evil-mc-mode t)

  (dolist (mode '(vterm-mode
                  ranger-mode
                  elpaca-ui-mode
                  message-mode
                  special-mode
                  dap-ui-breakpoints-ui-list-mode
                  eglot-list-connections-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(defun editor/base--non-prog-mode-setup ()
  "Setup to run for non `prog-mode` major modes."
  (unless (derived-mode-p 'prog-mode)
    (setq truncate-lines nil)
    (visual-line-mode 1)))

(defun editor/base--general-buffer-setup ()
  "Set up general buffer configurations."
  (add-hook 'before-save-hook #'delete-trailing-whitespace)
  (add-hook 'find-file-hook #'editor/base--read-only-by-prefix)
  (add-hook 'after-change-major-mode-hook 'editor/base--non-prog-mode-setup))

(defun editor/base--editorconfig-setup ()
  "Set up editorconfig tool configurations."
  (setq-default editorconfig-lisp-use-default-indent t)

  (editorconfig-mode 1))

(defun editor/base-setup ()
  "Set up editor configurations."
  (editor/base--general-buffer-setup)
  (editor/base--editorconfig-setup)
  (editor/base--evil-mode-setup)

  (setq-default display-line-numbers-type 'relative)
  (editor/input--set-english-input-method))

(editor/base-setup)

(provide 'editor-base)

;;; editor-base.el ends here
