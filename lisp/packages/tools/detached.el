;;; detached.el -*- lexical-binding: t; -*-

(require 'util-commands)

(defun detached-project-compile ()
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (call-interactively #'detached-compile)))

(defun detached-project-recompile (&optional edit-command)
  (interactive "P")
  (let ((compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             ;; Should we error instead?  When there's no
             ;; project-specific naming, there is no point in using
             ;; this command.
             compilation-buffer-name-function)))
    (detached-compile-recompile edit-command)))

(use-package detached
  :custom
  (detached-terminal-data-command system-type)
  (detached-list-display-buffer-action nil)
  (detached-list-open-session-display-buffer-action nil)
  (detached-open-session-display-buffer-action nil)
  (detached-session-info-buffer-action nil)
  (detached-notification-function #'+detach-notifications-message)
  (util/commands-string-command-function #'detached-shell-command)
  :init
  (setq detached-init-package-integration '((compile . detached-init--compile)
                                            (dired . detached-init--dired)
                                            (dired-rsync . detached-init--dired-rsync)
                                            (embark . detached-init--embark)
                                            (eshell . detached-init--eshell)
                                            (org . detached-init--org)
                                            (shell . detached-init--shell)))
  (detached-init)
  :config
  (setq detached-session-mode nil)

  (defun +detach-notifications-message (session)
    "Issue a notification when SESSION transitions from active to inactive.
This function uses the `notifications' library."
    (let ((status (detached-session-status session))
          (host (detached-session-host-name session)))
      (notifications-notify
       :title (pcase status
                ('success (format "Detached finished [%s]" host))
                ('failure (format "Detached failed [%s]" host)))
       :body (detached-session-command session)
       :urgency (pcase status
                  ('success 'normal)
                  ('failure 'normal)))))

  (defun +detached-list-open-session ()
    "View session."
    (interactive)
    (let ((session (tabulated-list-get-id))
          (detached-open-session-display-buffer-action
           detached-list-open-session-display-buffer-action))
      (when-let ((single-window (> (length (window-list)) 1))
                 (buffer (current-buffer)))
        (bury-buffer buffer))
      (detached-open-session session)))

  (advice-add #'detached-list-open-session :override #'+detached-list-open-session)

  :bind (([remap async-shell-command]   . util/commands-run-command)
         ([remap detached-open-session] . detached-consult-session))
  )
