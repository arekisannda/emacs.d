;;; ui-minibuffer.el --- Emacs Minibuffer Configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)

(defun packages/vertico--sort-directories-first (files)
  "Sort FILES by directories first."
  (setq files (vertico-sort-history-length-alpha files))
  (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
         (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

;; Disable preview for consult-grep commands
(consult-customize consult-ripgrep consult-git-grep consult-grep :preview-key nil)

(defun ui/minibuffer-setup ()
  "Set up minibuffer configurations."
  ;; (setq which-key-posframe-poshandler 'posframe-poshandler-window-bottom-left-corner)
  ;; (which-key-posframe-mode 1)
  (setq-default which-key-sort-order 'which-key-description-order)
  (which-key-mode 1)

  (vertico-mode 1)
  (vertico-multiform-mode 1)
  (savehist-mode 1)
  (marginalia-mode 1)

  (setq vertico-multiform-commands
        `((find-file (vertico-sort-override-function . vertico-sort-alpha))
          (project-switch-project (vertico-sort-override-function . packages/vertico--sort-directories-first))
          (project-kill-buffers (vertico-sort-override-function . packages/vertico--sort-directories-first))
          (project-find-file (vertico-sort-override-function . packages/vertico--sort-directories-first))
          (project-find-dir (vertico-sort-override-function . packages/vertico--sort-directories-first))
          (describe-symbol (vertico-sort-override-function . vertico-sort-alpha))))

  (setq vertico-multiform-categories
        `((file (vertico-sort-override-function . packages/vertico--sort-directories-first))
          (consult-grep buffer (vertico-buffer-display-action . (display-buffer-same-window)))))

  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(provide 'ui-minibuffer)

;;; ui-minibuffer.el ends here
