;;; editor.el --- Emacs editor related packages and configurations -*- lexical-binding: t; origami-fold-style: triple-braces; -*-
;;; Commentary:

;;; Code:

;;; evil-mode {{{
(use-package evil
  :after general
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-default-state 'normal
        evil-want-C-u-jump nil
        evil-want-C-u-scroll t
        evil-want-keybinding nil
        evil-undo-system 'undo-fu
        evil-want-minibuffer nil)
  :config
  (general-evil-setup t)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :init
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
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter :ensure t)
(use-package evil-args :ensure t)

(use-package evil-snipe
  :after evil
  :init
  (setq evil-snipe-enable-highlight t)
  :config
  (evil-snipe-override-mode +1))

(use-package evil-easymotion
  :after evil)

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-lion
  :ensure t
  :init
  (setq evil-lion-squeeze-spaces t
	    evil-lion-left-align-key nil
	    evil-lion-right-align-key nil)
  :config
  (evil-lion-mode 1))
;;; }}}

;;; disable mouse {{{
(use-package disable-mouse
  :ensure t
  :config
  (disable-mouse-global-mode 1))
;;; }}}

;;; modeline {{{
(use-package telephone-line
  :config
  (telephone-line-mode 1))

(use-package diminish
  :ensure t
  :config
  (dolist (mode '(evil-collection-unimpaired-mode
                  auto-revert-mode
                  projectile-mode
                  disable-mouse-mode
                  company-mode))
    (diminish mode)))

;;; }}}

;;; themes {{{
(use-package sonokai-theme
  :ensure t
  :elpaca (:host github :repo "arekisannda/sonokai-emacs")
  :config
  (load-theme 'sonokai t))
;;; }}}

;;; fonts {{{
(use-package nerd-icons :ensure t)

(use-package rainbow-mode)
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package mixed-pitch
  :ensure t
  :hook
  (org-mode . mixed-pitch-mode)
  :init
  (setq mixed-pitch-set-height configs--variable-pitch-font-size))
;;; }}}

;;; undo-tree {{{
(use-package undo-fu :ensure t :after diminish)
;;; }}}

;;; syntax check {{{
(use-package flycheck
  :ensure t
  :init
  (setq flycheck-indication-mode nil
        flycheck-mode-line nil
        flycheck-display-errors-delay 3600.0
        flycheck-check-syntax-automatically '(mode-enabled save))
  :config
  (global-flycheck-mode 1))

(use-package flycheck-posframe
  :after flycheck
  :ensure t)

(use-package flycheck-popup-tip
  :after flycheck
  :ensure t)
;;; }}}

;;; code styles {{{
(use-package editorconfig
  :ensure t
  :init
  (setq editorconfig-lisp-use-default-indent t)
  :config
  (editorconfig-mode 1))
;;; }}}

;;; code folding {{{
(use-package origami
  :ensure t
  :init
  (setq origami-show-fold-header t)
  :config
  (global-origami-mode t))

(use-package lsp-origami
  :after origami
  :hook (lsp-after-open-hook . lsp-origami-try-enable))
;;; }}}

;;; code completion {{{
(use-package company
  :init
  (setq company-manual-begin t
        company-idle-delay nil
        company-tooltip-minimum-width 40
        company-tooltip-align-annotations t)
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-posframe
  :after company)

(use-package company-flx
  :after company
  :config
  (company-flx-mode +1))

(use-package company-auctex
  :after (auctex company)
  :config
  (company-auctex-init))

(use-package company-math
  :after company)

(use-package company-reftex
  :after company)
;;; }}}

(provide 'configs-editor)
;;; editor.el ends here
