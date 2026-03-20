;;; ui/dashboard.el -*- lexical-binding: t; -*-

(setq +dashboard-widget-actions
      '((recents    . consult-recent-file)
        (bookmarks  . consult-bookmark)
        (projects   . project-switch-project)
        (files      . find-file)
        (workspaces . activities-resume)
        (git        . magit)
        (agenda     . (lambda () (interactive) (org-agenda nil "a")))
        (configs    . (lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))))))

(defun +dashboard-get-action (item)
  "Get the action to be used for ITEM."
  (let ((elem (assq item +dashboard-widget-actions)))
    (and elem (cdr elem))))

(defun +dashboard-widget-tag (str)
  "Format STR for dashboard widget."
  (format "%-85s" str))

(defmacro +dashboard-insert-shortmenu (item &rest args)
  "Add dashboard ITEM shortmenu.
The optional ARGS are keyword arguments."
  `(progn
     (insert (format "%s " (nerd-icons-octicon ,(plist-get args :icon) :face 'dashboard-heading)))
     (widget-create 'item
                    :tag (+dashboard-widget-tag ,(plist-get args :title))
                    :action (lambda (&rest _) (call-interactively (+dashboard-get-action ,item)))
                    :mouse-face 'highlight
                    :button-face 'dashboard-heading
                    :button-prefix ""
                    :button-suffix ""
                    :format "%[%t%]")
     (insert (propertize ,(plist-get args :shortcut)
                         'face
                         'font-lock-constant-face))))

(defun +dashboard-insert-file-shortmenu (&rest _)
  "Add file dashboard widget."
  (+dashboard-insert-shortmenu 'files
                               :title "Find File"
                               :icon "nf-oct-file"
                               :shortcut "f"))

(defun +dashboard-insert-project-shortmenu (&rest _)
  "Add project dashboard widget."
  (+dashboard-insert-shortmenu 'projects
                               :title "Open Project"
                               :icon "nf-oct-rocket"
                               :shortcut "p"))

(defun +dashboard-insert-workspace-shortmenu (&rest _)
  "Add workspace dashboard widget."
  (+dashboard-insert-shortmenu 'workspaces
                               :title "Open Workspace"
                               :icon "nf-oct-inbox"
                               :shortcut "w"))

(defun +dashboard-insert-org-agenda-shortmenu (&rest _)
  "Add Org agenda dashboard widget."
  (+dashboard-insert-shortmenu 'agenda
                               :title "Open Agenda"
                               :icon "nf-oct-calendar"
                               :shortcut "a"))

(defun +dashboard-insert-bookmark-shortmenu (&rest _)
  "Add bookmark dashboard widget."
  (+dashboard-insert-shortmenu 'bookmarks
                               :title "Jump to Bookmark"
                               :icon "nf-oct-bookmark"
                               :shortcut "b"))

(defun +dashboard-insert-recents-shortmenu (&rest _)
  "Add recent files dashboard widget."
  (dashboard-mute-apply
    (recentf-mode 1)
    (when dashboard-remove-missing-entry
      (ignore-errors (recentf-cleanup))))
  (+dashboard-insert-shortmenu 'recents
                               :title "Recent Files"
                               :icon "nf-oct-history"
                               :shortcut "r"))

(defun +dashboard-insert-user-configs-shortmenu (&rest _)
  "Add configuration file dashboard widget."
  (+dashboard-insert-shortmenu 'configs
                               :title "Emacs Configurations"
                               :icon "nf-oct-gear"
                               :shortcut "c"))

(defun +dashboard-insert-git-dash-shortmenu (&rest _)
  "Add Git dashboard widget."
  (+dashboard-insert-shortmenu 'git
                               :title "Git Dashboard"
                               :icon "nf-oct-git_branch"
                               :shortcut "g"))

(use-package dashboard
  :custom
  (initial-buffer-choice 'dashboard-open)
  (dashboard-buffer-name " *dashboard*")
  (dashboard-icon-type 'nerd-icons)
  (dashboard-startup-banner (expand-file-name "assets/logo.png" user-emacs-directory))
  (dashboard-banner-logo-title nil)
  (dashboard-projects-backend 'project-el)
  (dashboard-vertically-center-content t)
  (dashboard-center-content t)
  (dashboard-show-shortcuts t)
  (dashboard-display-icons-p t)
  (dashboard-set-heading-icons t)
  (dashboard-set-footer nil)
  (dashboard-set-file-icons t)
  (dashboard-show-shortcuts nil)

  (dashboard-startupify-list
   '(dashboard-insert-newline
     dashboard-insert-items
     dashboard-insert-init-info))

  (dashboard-items
   '((agenda . 15)
     bookmarks
     workspaces
     projects
     files
     recents
     git
     configs))

  (dashboard-item-generators
   '((configs . +dashboard-insert-user-configs-shortmenu)
     (recents . +dashboard-insert-recents-shortmenu)
     (workspaces . +dashboard-insert-workspace-shortmenu)
     (git . +dashboard-insert-git-dash-shortmenu)
     (bookmarks . +dashboard-insert-bookmark-shortmenu)
     (projects . +dashboard-insert-project-shortmenu)
     (files . +dashboard-insert-file-shortmenu)
     (agenda . dashboard-insert-agenda)))
  :hook
  (after-init . dashboard-insert-startupify-lists)
  (after-init . dashboard-setup-startup-hook))
