;;; editor.el --- Emacs editor related packages and configurations -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; additional icons
(use-package nerd-icons)

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-banner-logo-title nil
        dashboard-startup-banner (expand-file-name "logo.png" user-emacs-directory)
        dashboard-center-content t
        dashboard-show-shortcuts t
        dashboard-set-footer nil
        dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-projects-backend 'project-el
        dashboard-items '((agenda . 8)  (bookmarks . 8) (projects . 16) (recents . 32)))

  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (dashboard-setup-startup-hook))

(use-package editorconfig
  :ensure t
  :init
  (setq editorconfig-lisp-use-default-indent t)
  :config
  (editorconfig-mode 1))

(use-package ibuffer-project
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups)))))

;; window navigation
(use-package ace-window
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; editor enhancements
(use-package rainbow-mode)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; packages for disabling some emacs features
(use-package disable-mouse
  :ensure t
  :hook (prog-mode . disable-mouse-mode))

(use-package telephone-line
  :config
  (telephone-line-mode 1))

;; syntax check
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

;; completion
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

(provide 'packages-editor)

;;; editor.el ends here
