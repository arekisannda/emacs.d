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

;; window nvaigation
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

(provide 'packages-editor)

;;; editor.el ends here
