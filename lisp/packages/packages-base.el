;;; packages-base.el --- Base Packages Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'lib-buffer-extras)

(use-package aio)

(use-package shut-up)

(use-package no-littering)

(use-package diminish)

(use-package undo-fu)

(use-package llama)

(use-package epkg :after llama)

(use-package general)

(use-package hydra)

(use-package compat)

(use-package persist)

(use-package ov)

(setq-default evil-respect-visual-line-mode t
              evil-want-keybinding nil
              evil-want-minibuffer nil)

(use-package evil :after undo-fu
  :custom
  (evil-want-integration t)
  (evil-default-state 'normal)
  (evil-undo-system 'undo-fu)
  (evil-split-window-below nil)
  (evil-vsplit-window-right nil)
  (+buffer-scroll-left-function #'evil-scroll-column-left)
  (+buffer-scroll-right-function #'evil-scroll-column-right)
  (+buffer-scroll-up-function #'evil-scroll-line-up )
  (+buffer-scroll-down-function #'evil-scroll-line-down)
  :config
  (setq evil-insert-state-modes
        '(comint-mode erc-mode geiser-repl-mode gud-mode inferior-apl-mode inferior-caml-mode
                      inferior-emacs-lisp-mode inferior-j-mode inferior-python-mode inferior-scheme-mode inferior-sml-mode
                      internal-ange-ftp-mode haskell-interactive-mode prolog-inferior-mode racket-repl-mode reb-mode shell-mode
                      slime-repl-mode term-mode utop-mode wdired-mode))

  (setq evil-emacs-state-modes
        (delete-dups
         (append '(vterm-mode
                   eshell-mode
                   special-mode
                   dap-ui-breakpoints-ui-list-mode
                   dape-repl-mode
                   calc-mode
                   comint-mode
                   calculator-mode
                   calendar-mode
                   eglot-list-connections-mode
                   inferior-python-mode)
                 evil-emacs-state-modes)))

  (setq evil-motion-state-modes
        '(apropos-mode
          color-theme-mode
          command-history-mode
          messages-buffer-mode
          backtrace-mode
          compilation-mode
          dictionary-mode
          ert-results-mode
          help-mode
          helpful-mode
          Info-mode
          devdocs-mode
          Man-mode
          speedbar-mode
          embark-collect-mode
          undo-tree-visualizer-mode
          rfc-mode
          woman-mode))
  :hook
  (after-init . evil-mode))

(use-package evil-collection :after evil
  :custom
  (evil-collection-mode-list
   '(info
     dired
     ibuffer
     magit
     forge
     edebug
     org
     org-roam
     ediff))
  :diminish evil-collection-unimpaired-mode
  :init
  (setq forge-add-default-bindings nil)
  :hook
  (evil-mode . evil-collection-init))

(use-package evil-nerd-commenter :after evil)

(use-package evil-args :after evil)

(use-package evil-snipe :after evil
  :diminish evil-snipe-mode
  :custom
  (evil-snipe-enable-highlight t)
  :hook
  (evil-mode . evil-snipe-mode))

(use-package evil-easymotion :after evil)

(use-package evil-matchit :after evil
  :hook
  (evil-mode . global-evil-matchit-mode))

(use-package evil-mc :after evil
  :init
  (setq evil-mc-cursors-map (make-sparse-keymap)
        evil-mc-key-map (make-sparse-keymap))
  :hook
  (evil-mode . global-evil-mc-mode))

(use-package embrace
  :custom-face
  (embrace-help-separator-face
   ((nil :inherit font-lock-comment-face)))
  (embrace-help-key-face
   ((nil :inherit font-lock-function-name-face)))
  (embrace-help-mark-func-face
   ((nil :inherit font-lock-constant-face)))
  (embrace-help-pair-face
   ((nil :inherit nil
         :foreground  ,(doom-color 'blue)
         :inverse-video nil)))
  :init
  (setq embrace-help-separator " : ")
  (setq embrace--help-add-column-width 2)
  (setq embrace-show-help-p t)
  :config
  (defun +embrace--show-help-buffer (help-string)
    (let ((alist '((window-width  . (lambda (w) (fit-window-to-buffer w 20 1)))
                   (window-height . (lambda (w) (fit-window-to-buffer w 20 1))))))
      (embrace--setup-help-buffer)
      (with-current-buffer embrace--help-buffer
        (erase-buffer)
        (insert help-string)
        (goto-char (point-min)))
      (if (get-buffer-window embrace--help-buffer)
          (display-buffer-reuse-window embrace--help-buffer alist)
        (let ((w (split-window (frame-root-window nil) nil nil)))
          (window--display-buffer embrace--help-buffer w 'window alist)
          (set-window-dedicated-p w t)
          (fit-window-to-buffer w))
        )))

  (advice-add #'embrace--show-help-buffer :override #'+embrace--show-help-buffer)
  )

(use-package editorconfig
  :config
  (setq editorconfig-lisp-use-default-indent t)
  (editorconfig-mode t)
  :diminish editorconfig-mode)

(defvar-local +envrc-update-hook '()
  "Buffer-local hook to run after `envrc--update'.")

(defun +envrc--update-after-setup ()
  "Setup to run after `envrc--update'."
  (run-hooks '+envrc-update-hook))

(use-package envrc
  :custom
  (envrc-show-summary-in-minibuffer nil)
  :hook
  (after-init . envrc-global-mode)
  :init
  (advice-add #'envrc--update :after #'+envrc--update-after-setup))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "SauceCodePro Nerd Font Mono"))

(use-package posframe
  :custom
  (posframe-inhibit-double-buffering t)
  (posframe-mouse-banish-function #'posframe-mouse-banish-default)
  :config
  (defun +posframe-show-refresh (buffer &rest _)
    (posframe-refresh buffer))
  (advice-add #'posframe-show :after #'+posframe-show-refresh))

(use-package transient
  :custom
  (transient-show-popup t)
  (transient-display-buffer-action
   '(display-buffer-in-side-window
     (side . right)
     (slot . 1)
     (dedicated . t)))
  (transient-mode-line-format nil)
  (transient-force-fixed-pitch t))

(use-package which-key
  :custom
  (which-key-popup-type 'custom)
  (which-key-sort-order 'which-key-description-order)
  (which-key-show-prefix 'echo)
  (which-key-side-window-slot 0)
  (which-key-side-window-location 'bottom)
  (which-key-max-display-columns nil)
  (which-key-side-window-max-width 0)
  (which-key-min-column-description-width 30)
  (which-key-custom-hide-popup-function
   (lambda ()
     (when (buffer-live-p which-key--buffer)
       ;; in case which-key buffer was shown in an existing window, `quit-window'
       ;; will re-show the previous buffer, instead of closing the window
       (quit-windows-on which-key--buffer)
       (when (and which-key-preserve-window-configuration
                  which-key--saved-window-configuration)
         (set-window-configuration which-key--saved-window-configuration)
         (setq which-key--saved-window-configuration nil)))))

  (which-key-custom-show-popup-function #'+which-key--show-buffer-root-window)

  (which-key-custom-popup-max-dimensions-function
   (lambda (&optional width)
     (cons
      ;; height
      30
      ;; width
      (let ((edges (window-edges (window-main-window))))
        (- (nth 2 edges) (nth 0 edges)))
      )))
  :config
  (defun +which-key--show-buffer-root-window (&optional act-popup-dim)
    (when (and which-key-preserve-window-configuration
               (not which-key--saved-window-configuration))
      (setq which-key--saved-window-configuration (current-window-configuration)))
    (let* ((alist `((window-width  . (lambda (w) (fit-window-to-buffer w 20 1)))
                    (window-height . (lambda (w) (fit-window-to-buffer w 20 1))))))
      (cond
       ((eq which-key--multiple-locations t)
        (delete-windows-on which-key--buffer)
        (let ((w (split-window (frame-root-window nil) nil nil)))
          (window--display-buffer which-key--buffer w 'window alist)
          (fit-window-to-buffer w)
          ))
       ((get-buffer-window which-key--buffer)
        (display-buffer-reuse-window which-key--buffer alist))
       (t
        (let ((w (split-window (frame-root-window nil) nil nil)))
          (window--display-buffer which-key--buffer w 'window alist)
          (set-window-dedicated-p w t)
          w)
        )
       )))
  :hook
  (after-init . which-key-mode))

(provide 'packages-base)

;;; packages-base.el ends here
