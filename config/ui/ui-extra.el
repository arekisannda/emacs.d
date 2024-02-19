;;; ui-extra.el --- Extra UI Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'packages-init)

(setq-default ui/extra--logo-path (expand-file-name "logo.png" user-emacs-directory))

(defun ui/extra--dashboard-setup ()
  "Set up dashboard configuations."
  (setq dashboard-startup-banner ui/extra--logo-path
        dashboard-banner-logo-title nil
        dashboard-projects-backend 'project-el
        dashboard-center-content t
        dashboard-show-shortcuts t
        dashboard-display-icons-p t
        dashboard-set-heading-icons t
        dashboard-set-footer nil
        dashboard-set-file-icons t
        dashboard-items '((agenda . 8)  (bookmarks . 8) (projects . 16) (recents . 32))
        initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (dashboard-setup-startup-hook))

(defun ui/extra--treemacs-setup ()
  "Set up Treemacs configurations."
  (require 'treemacs-persp)
  (treemacs-load-theme "nerd-icons")
  (treemacs-set-scope-type 'Perspectives))

(defun ui/extra--fringe-setup ()
  "Set up fringe configurations."
  (setq-default fringe-styles 'default)
  (setq-default fringe-indicator-alist nil)

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (unless (display-graphic-p)
                      (diff-hl-margin-mode 1))))))

  (diff-hl-flydiff-mode 1)
  (global-diff-hl-mode nil)
  (fringe-mode 5))

(defun ui/extra--ranger-setup ()
  "Set up Ranger configurations."
  (ranger-override-dired-mode 1))

(defun ui/extra-setup ()
  "Set up extra ui tools configurations"
  (ui/extra--fringe-setup)
  (ui/extra--dashboard-setup)
  (ui/extra--treemacs-setup)
  (ui/extra--ranger-setup))

(provide 'ui-extra)

;;; ui-extra.el ends here
