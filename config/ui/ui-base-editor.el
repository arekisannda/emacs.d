;;; ui-base-editor.el --- Emacs Base Editor Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)

(defvar ui/base-editor--read-only-prefixes-list
  (list (expand-file-name elpaca-directory)
        (expand-file-name package-user-dir))
  "List of read-only file prefixes.")

(defun ui/base-editor--read-only-by-prefix ()
  "Enable `read-only-mode` if buffer includes one of `ui/base-editor--read-only-prefixes-list`."
  (when (and buffer-file-name
             (or (cl-loop for prefix in ui/base-editor--read-only-prefixes-list
                          thereis (string-prefix-p prefix buffer-file-name))))
    (read-only-mode 1)))

(defun ui/base-editor--evil-mode-setup ()
  "Set up evil-mode packages."
  (setq evil-collection-mode-list
        '(dashboard
          info
          dired
          ibuffer
          magit
          edebug
          org
          org-roam
          ediff))

  (dolist (mode '(vterm-mode
                  ranger-mode
                  elpaca-ui-mode
                  message-mode
                  special-mode
                  dap-ui-breakpoints-ui-list-mode
                  eglot-list-connections-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (evil-mode 1)
  (evil-collection-init)
  (evil-snipe-override-mode +1)
  (global-evil-matchit-mode 1)
  (evil-lion-mode 1)
  (global-evil-mc-mode t))

(defun ui/base-editor--visual-line-mode-setup ()
  "Setup to run for non `prog-mode` major modes."
  (setq truncate-lines nil)
  (visual-line-mode 1))

(defun ui/base-editor--exec-on-save ()
  "Operations to be executed on buffer save."
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max)))

(defun ui/base-editor--general-buffer-setup ()
  "Set up general buffer configurations."
  (add-hook 'before-save-hook #'ui/base-editor--exec-on-save)
  (add-hook 'find-file-hook #'ui/base-editor--read-only-by-prefix)
  (add-hook 'help-mode-hook #'ui/base-editor--visual-line-mode-setup))

(defun ui/base-editor--editorconfig-setup ()
  "Set up editorconfig tool configurations."
  (setq-default editorconfig-lisp-use-default-indent t)

  (editorconfig-mode 1))

(defun ui/base-editor-setup ()
  "Set up editor configurations."
  (setq-default display-line-numbers-type 'relative)
  (setq-default hscroll-step 5)
  (setq-default scroll-step 5)
  (setq-default tab-width 4)

  (ui/base-editor--general-buffer-setup)
  (ui/base-editor--editorconfig-setup)
  (ui/base-editor--evil-mode-setup))

(provide 'ui-base-editor)

;;; ui-base-editor.el ends here
