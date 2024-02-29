;;; packages-interface.el --- Interface Packages Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package nerd-icons)

(use-package mixed-pitch :disabled)

(use-package ranger :after evil
  :hook
  (emacs-startup . ranger-override-dired-mode))

(use-package rainbow-mode
  :custom
  (rainbow-r-colors-alist '())
  (rainbow-html-colors-alist '()))

(use-package rainbow-delimiters)

(use-package telephone-line
  :hook
  (elpaca-after-init . telephone-line-mode))

(use-package ace-window :after posframe
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-dispatch-when-more-than 0)
  :hook
  (window-setup . ace-window-posframe-mode)
  (server-after-make-frame . ace-window-posframe-mode))

(use-package dashboard :after nerd-icons
  :custom
  (dashboard-icon-type 'nerd-icons)
  (dashboard-startup-banner (expand-file-name "assets/logo.png" user-emacs-directory))
  (dashboard-banner-logo-title nil)
  (dashboard-projects-backend 'project-el)
  (dashboard-center-content t)
  (dashboard-show-shortcuts t)
  (dashboard-display-icons-p t)
  (dashboard-set-heading-icons t)
  (dashboard-set-footer nil)
  (dashboard-set-file-icons t)
  (dashboard-items '((agenda . 8)  (bookmarks . 8) (projects . 16) (recents . 32)))
  (initial-buffer-choice #'dashboard-open)
  :hook
  (emacs-startup . dashboard-setup-startup-hook)
  (emacs-startup . dashboard-insert-startupify-lists))

(provide 'packages-interface)

;;; packages-interface.el ends here
