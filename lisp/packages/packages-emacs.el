;;; packages-emacs.el --- Emacs Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package emacs
  :ensure nil
  ;; :diminish auto-revert-mode
  :custom
  (display-line-numbers-type 'relative)
  :preface
  (defun +emacs-tuning-configurations ()
    ;; performance tuning
    (defvar packages/emacs-gc-cons-threshold (* 1024 1024 100))
    (setq gc-cons-threshold packages/emacs-gc-cons-threshold)
    (setq read-process-output-max (* 1024 1024)))

  (defun +emacs-configurations ()
    (setq-default window-resize-pixelwise t)
    (setq-default frame-resize-pixelwise t)
    (setq-default hscroll-step 5)
    (setq-default scroll-step 5)
    (setq-default tab-width 4)
    (setq-default tab-bar-separator "")

    (setq-default fringe-styles 'default)
    (setq-default fringe-indicator-alist nil)
    (fringe-mode 5))

  (defvar +emacs-read-only-prefixes-list
    (list (expand-file-name elpaca-directory)
          (expand-file-name package-user-dir))
    "List of read-only file prefixes.")

  (defun +emacs-set-read-only-by-prefix ()
    "Enable `read-only-mode` if buffer includes one of `+emacs-read-only-prefixes-list`."
    (when (and buffer-file-name
               (cl-loop for prefix in +emacs-read-only-prefixes-list
                        thereis (string-prefix-p prefix buffer-file-name)))
      (read-only-mode 1)))

  (defun +emacs-set-visual-line-mode ()
    "Setup to run for non `prog-mode` major modes."
    (setq truncate-lines nil)
    (visual-line-mode 1))

  (defun +emacs-exec-on-save ()
    "Operations to be executed on buffer save."
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max)))

  (defun +emacs-minibuffer-setup ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun +emacs-minibuffer-exit ()
    (setq gc-cons-threshold packages/emacs-gc-cons-threshold))

  (defun +emacs-evil-mode-setup ()
    (dolist (mode '(vterm-mode
                    ranger-mode
                    elpaca-ui-mode
                    message-mode
                    special-mode
                    dap-ui-breakpoints-ui-list-mode
                    eglot-list-connections-mode))
      (add-to-list 'evil-emacs-state-modes mode)))

  (defun +emacs-load-keybinds ()
    (require 'keybinds-global)
    (require 'keybinds-evil)
    (require 'keybinds-session)
    (require 'keybinds-editor)
    (require 'keybinds-search)
    (require 'keybinds-completion))

  (defun +emacs-modeline-setup ()
    (require 'telephone-line-utils)
    (telephone-line-defsegment* +popper-telephone-line-tag-segment ()
      (if popper-popup-status "󰊠" nil))

    (setq telephone-line-lhs '((evil   . (+popper-telephone-line-tag-segment
                                          telephone-line-evil-tag-segment))
                               (accent . (telephone-line-vc-segment
                                          telephone-line-erc-modified-channels-segment
                                          telephone-line-process-segment))
                               (nil    . (telephone-line-projectile-segment
                                          telephone-line-buffer-segment)))))
  :hook
  (before-save . +emacs-exec-on-save)
  (find-file . +emacs-set-read-only-by-prefix)
  (help-mode . +emacs-set-visual-line-mode)
  (elpaca-after-init . (lambda () (load custom-file 'noerror)))
  (minibuffer-setup . +emacs-minibuffer-setup)
  (minibuffer-exit . +emacs-minibuffer-exit)
  (emacs-startup . +emacs-tuning-configurations)
  (emacs-startup . +emacs-configurations)
  (elpaca-after-init . +emacs-evil-mode-setup)
  (emacs-startup . +emacs-load-keybinds))

(provide 'packages-emacs)

;;; packages-emacs.el ends here
