;;; management-workspace.el --- Emacs Workspace Management Configurations -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'ibuf-ext)
(require 'project)
(require 'packages-init)
(require 'ui-tab-bar)

(defvar management/workspace-project-name nil)
(defvar management/workspace-project-update-hook nil)
(defvar management/workspace-project-clear-hook nil)

(defun management/workspace--project-setup ()
  "Set up project configurations."
  (setq project-vc-extra-root-markers '(".project.el" ".projectile" ".project")
        project-vc-include-untracked nil
        project-vc-merge-submodules nil))

(defun management/workspace--ibuffer-setup ()
  "Set up ibuffer configurations."
  (add-hook 'ibuffer-hook
            (lambda ()
              (setq ibuffer-filter-groups
                    (ibuffer-project-generate-filter-groups)))))

(defun management/workspace--persp-mode-project-setup ()
  "Add `persp-mode` hooks to save and restore project name configurations."
  (advice-add 'project-switch-project :after
              (lambda (&rest _) ; set project name variable
                (let ((project-name (project-root (project-current))))
                  (setq management/workspace-project-name project-name)
                  (run-hook-with-args 'management/workspace-project-update-hook
                                      project-name))))

  (advice-add 'project-kill-buffers :after
              (lambda (&rest _) ; set project name variable
                (setq management/workspace-project-name nil)
                (run-hook-with-args 'management/workspace-project-update-hook nil)))

  (add-hook 'persp-activated-functions
            (lambda (_) ; load project name
              (let ((project-name (persp-parameter 'proj-name)))
                (setq management/workspace-project-name project-name)
                (run-hook-with-args 'management/workspace-project-update-hook
                                    project-name))))

  (add-hook 'persp-before-deactivate-functions
            (lambda (_) ; save project name
              (when (get-current-persp)
                (set-persp-parameter 'proj-name management/workspace-project-name))))

  (add-hook 'persp-before-save-state-to-file-functions
            (lambda (&rest _) ; save project name
              (set-persp-parameter 'proj-name management/workspace-project-name))))

(defun management/workspace--persp-mode-tab-bar-setup ()
  "Add `persp-mode` hooks to save and restore tab-bar configurations."
  (add-hook 'persp-before-deactivate-functions
            (lambda (_) ; save tab-bar data
              (when (get-current-persp)
                (set-persp-parameter 'tab-bar-tabs (tab-bar-tabs)))))

  (add-hook 'persp-activated-functions
            (lambda (_) ; load tab-bar data
              (tab-bar-tabs-set (persp-parameter 'tab-bar-tabs))
              (tab-bar--update-tab-bar-lines t)))

  (add-hook 'persp-before-save-state-to-file-functions
            (lambda (&rest _) ; save tab-bar data
              (when (get-current-persp)
                (set-persp-parameter
                 'tab-bar-tabs
                 (frameset-filter-tabs (tab-bar-tabs) nil nil t))))))

(defun management/workspace--persp-mode-setup ()
  "Set up `persp-mode` configurations."
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook
                #'(lambda () (unless persp-mode (persp-mode 1))))
    (add-hook 'window-setup-hook
              #'(lambda () (unless persp-mode (persp-mode 1)))))

  (add-hook 'persp-mode-hook #'(lambda ()
                                 (persp-hide "system")
                                 (persp-switch "main")))

  (add-hook 'persp-mode-hook #'management/workspace--persp-mode-project-setup)
  (add-hook 'persp-mode-hook #'management/workspace--persp-mode-tab-bar-setup)

  (add-hook 'persp-activated-functions
            (lambda (_) ; set frame title
              (let ((current (safe-persp-name (get-current-persp)))
                    (debug-prefix (if init-file-debug "DEBUG: " "")))
                (set-frame-name (format "%s%s" debug-prefix current)))))

  (add-hook 'persp-renamed-functions
            (lambda (_ _ new) ; set frame title
              (let ((debug-prefix (if init-file-debug "DEBUG: " "")))
                (set-frame-name (format "%s%s" debug-prefix new)))))
  )

(defun management/workspace-setup ()
  "Set up workspace configurations."
  (management/workspace--project-setup)
  (management/workspace--ibuffer-setup)
  (management/workspace--persp-mode-setup))

(management/workspace-setup)

(provide 'management-workspace)

;;; management-workspace.el ends here
