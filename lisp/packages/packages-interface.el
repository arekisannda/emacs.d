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
  :preface
  (defun +dashboard-insert-project-shortmenu (&rest _)
    (let* ((fn #'project-switch-project)
           (fn-keymap (format "\\[%s]" fn))
           (icon-name (alist-get 'projects dashboard-heading-icons))
           (icon (nerd-icons-octicon icon-name :face 'dashboard-heading)))
      (insert (format "%s " icon))
      (widget-create 'item
                     :tag (format "%-30s" "Open project")
                     :action (lambda (&rest _) (call-interactively #'project-switch-project))
                     :mouse-face 'highlight
                     :button-face 'dashboard-heading
                     :button-prefix ""
                     :button-suffix ""
                     :format "%[%t%]")
      (insert (propertize (substitute-command-keys fn-keymap)
                          'face
                          'font-lock-constant-face))))

  (defun +dashboard-insert-org-agenda-shortmenu (&rest _)
    (let* ((fn #'org-agenda)
           (fn-keymap (format "\\[%s]" fn))
           (icon-name (alist-get 'agenda dashboard-heading-icons))
           (icon (nerd-icons-octicon icon-name :face 'dashboard-heading)))
      (insert (format "%s " icon))
      (widget-create 'item
                     :tag (format "%-30s" "Open org-agenda")
                     :action (lambda (&rest _) (call-interactively #'org-agenda))
                     :mouse-face 'highlight
                     :button-face 'dashboard-heading
                     :button-prefix ""
                     :button-suffix ""
                     :format "%[%t%]")
      (insert (propertize (substitute-command-keys fn-keymap)
                          'face
                          'font-lock-constant-face))))

  (defun +dashboard-insert-bookmark-shortmenu (&rest _)
    (let* ((fn #'consult-bookmark)
           (fn-keymap (format "\\[%s]" fn))
           (icon-name (alist-get 'bookmarks dashboard-heading-icons))
           (icon (nerd-icons-octicon icon-name :face 'dashboard-heading)))
      (insert (format "%s " icon))
      (widget-create 'item
                     :tag (format "%-30s" "Jump to bookmark")
                     :action (lambda (&rest _) (call-interactively #'consult-bookmark))
                     :mouse-face 'highlight
                     :button-face 'dashboard-heading
                     :button-prefix ""
                     :button-suffix ""
                     :format "%[%t%]")
      (insert (propertize (substitute-command-keys fn-keymap)
                          'face
                          'font-lock-constant-face))))

  (defun +dashboard-insert-recents-shortmenu (&rest _)
    (let* ((fn #'consult-recent-file)
           (fn-keymap (format "\\[%s]" fn))
           (icon-name (alist-get 'recents dashboard-heading-icons))
           (icon (nerd-icons-octicon icon-name :face 'dashboard-heading)))
      (insert (format "%s " icon))
      (widget-create 'item
                     :tag (format "%-30s" "Recently opened files")
                     :action (lambda (&rest _) (call-interactively #'consult-recent-file))
                     :mouse-face 'highlight
                     :button-face 'dashboard-heading
                     :button-prefix ""
                     :button-suffix ""
                     :format "%[%t%]")
      (insert (propertize (substitute-command-keys fn-keymap)
                          'face
                          'font-lock-constant-face))))
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
  (dashboard-startupify-list
   '(dashboard-insert-banner
     dashboard-insert-newline
     dashboard-insert-banner-title
     dashboard-insert-newline
     dashboard-insert-init-info
     dashboard-insert-items))

  ;; (dashboard-startupify-list
  ;;  '(dashboard-insert-banner
  ;;    dashboard-insert-newline
  ;;    dashboard-insert-banner-title
  ;;    dashboard-insert-newline
  ;;    dashboard-insert-init-info
  ;;    dashboard-insert-items))

  ;; (dashboard-item-generators
  ;;  '((recents . +dashboard-insert-recents-shortmenu)
  ;;    (bookmarks . +dashboard-insert-bookmark-shortmenu)
  ;;    (projects . +dashboard-insert-project-shortmenu)
  ;;    (agenda . +dashboard-insert-org-agenda-shortmenu)))

  ;; (dashboard-items
  ;;  '(agenda
  ;;    bookmarks
  ;;    projects
  ;;    recents))
  :hook
  (emacs-startup . dashboard-setup-startup-hook)
  (emacs-startup . dashboard-insert-startupify-lists))

(use-package treemacs)

(use-package treemacs-nerd-icons :after treemacs)

(use-package treemacs-evil :after treemacs)

(use-package treemacs-magit :after (treemacs magit))

(provide 'packages-interface)

;;; packages-interface.el ends here
