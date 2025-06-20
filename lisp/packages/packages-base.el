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
  (setq evil-emacs-state-modes
        (delete-dups
         (append '(vterm-mode
                   ranger-mode
                   elpaca-ui-mode
                   message-mode
                   special-mode
                   dap-ui-breakpoints-ui-list-mode
                   calc-mode
                   comint-mode
                   calculator-mode
                   calendar-mode
                   eglot-list-connections-mode
                   inferior-python-mode
                   eshell-mode)
                 evil-emacs-state-modes)))
  (setq evil-motion-state-modes
        '(apropos-mode
          color-theme-mode
          command-history-mode
          compilation-mode
          dictionary-mode
          ert-results-mode
          help-mode
          helpful-mode
          Info-mode
          Man-mode
          speedbar-mode
          undo-tree-visualizer-mode
          rfc-mode
          woman-mode))
  :hook
  (elpaca-after-init . evil-mode))

(use-package evil-collection :after evil
  :custom
  (evil-collection-mode-list
   '(info
     dired
     ibuffer
     magit
     edebug
     org
     org-roam
     ediff))
  :diminish evil-collection-unimpaired-mode
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

(use-package evil-lion :after evil
  :custom
  (evil-lion-squeeze-spaces t)
  (evil-lion-left-align-key nil)
  (evil-lion-right-align-key nil)
  :hook
  (evil-mode . evil-lion-mode))

(use-package evil-mc :after evil
  :init
  (setq evil-mc-cursors-map (make-sparse-keymap)
        evil-mc-key-map (make-sparse-keymap))
  :hook
  (evil-mode . global-evil-mc-mode))

(use-package embrace
  :init
  (setq embrace-show-help-p t)
  :config
  (defun +embrace-with-org-block ()
    (let ((block-type (completing-read
                       "Org block type: "
                       '(center comment example export justifyleft justifyright
                                quote src verse))))
      (cond ((string= block-type "src")
             (cons
              (concat (format "#+begin_src %s"
                              (completing-read "Language: "
                                               (embrace--get-org-src-block-modes)))
                      (let ((args (read-string "Arguments: ")))
                        (unless (string= args "")
                          (format " %s" args))))
              "#+end_src"))
            ((string= block-type "export")
             (cons (format "#+begin_export %s"
                           (completing-read "Format: "
                                            '(ascii beamer html latex texinfo)))
                   "#+end_export"))
            (t
             (setq block-type (downcase block-type))
             (cons (format "#+begin_%s" block-type)
                   (format "#+end_%s" block-type))))))

  (defun +embrace-org-mode-hook ()
    (dolist (lst '((?= "=" . "=")
                   (?~ "~" . "~")
                   (?/ "/" . "/")
                   (?* "*" . "*")
                   (?_ "_" . "_")
                   (?+ "+" . "+")
                   (?k "@@html:<kbd>@@" . "@@html:</kbd>@@")))
      (embrace-add-pair (car lst) (cadr lst) (cddr lst)))
    (embrace-add-pair-regexp ?l "#\\+begin_.*" "#\\+end_.*" 'embrace-with-org-block
                             (embrace-build-help "#+begin_*" "#+end") t))
  (advice-add #'embrace-with-org-block :override #'+embrace-with-org-block)
  (advice-add #'embrace-org-mode-hook :override #'+embrace-org-mode-hook)
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
  (elpaca-after-init . envrc-global-mode)
  :init
  (advice-add #'envrc--update :after #'+envrc--update-after-setup))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "SauceCodePro Nerd Font Mono"))

(provide 'packages-base)

;;; packages-base.el ends here
