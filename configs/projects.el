;;; projects.el --- Emeacs project management configurations -*- lexical-binding: t; origami-fold-style: triple-braces; -*-
;;; Commentary:

;;; Code:

(use-package project
  :elpaca nil
  :init
  (setq project-vc-extra-root-markers '(".project.el" ".projectile" ".project")
        project-vc-include-untracked nil
        project-vc-merge-submodules nil))

;; buffer by projects
(use-package ibuffer-project
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups)))))

;; perspectives
(use-package persp-mode
  :init
  (setq-default wg-morph-on nil)
  (setq-default persp-nil-name "main"
                persp-autokill-buffer-on-remove 'kill-weak
                persp-set-last-persp-for-new-frames nil
                persp-init-frame-behaviour nil
                persp-init-new-frame-behaviour-override nil
                persp-interactive-init-frame-behaviour-override nil
                persp-emacsclient-init-frame-behaviour-override nil)
  :config
  (add-hook 'persp-activated-functions
            (defun +workspace-set-frame-name (_)
              (let ((current (safe-persp-name (get-current-persp))))
                (set-frame-name (format "%s" current)))))

  (add-hook 'persp-before-deactivate-functions
	        (defun +workspaces-save-tab-bar-data-h (_)
	          (when (get-current-persp)
		        (set-persp-parameter 'tab-bar-tabs (tab-bar-tabs)))))

  (add-hook 'persp-activated-functions
            (defun +workspaces-load-tab-bar-data-h (_)
              (tab-bar-tabs-set (persp-parameter 'tab-bar-tabs))
              (tab-bar--update-tab-bar-lines t)))

  (add-hook 'persp-before-save-state-to-file-functions
            (defun +workspaces-save-tab-bar-data-to-file-h (&rest _)
              (when (get-current-persp)
                (set-persp-parameter
                 'tab-bar-tabs
                 (frameset-filter-tabs (tab-bar-tabs) nil nil t)))))

  (add-hook 'window-setup-hook #'(lambda () (persp-mode 1))))

;;; popper
(use-package popper
  :after persp-mode
  :init
  ;; set list of ephemeral buffers
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*Backtrace\\*"
          "\\*Customize.*\\*$"
          "\\*lsp-help\\*"
          "\\*compilation\\*"
          "\\*scratch\\*"
          "\\*info.*\\*"
          "\\*Man.*\\*$"
          "\\*elpaca.*\\*"
          "\\*Flycheck.*\\*$"
          "\\*Ediff Control.*\\*$"
          "\\*evil-marks\\*$"
          "^magit.*$"
          "\\*Async Shell Command\\*"
          "\\*EGLOT.*\\*$"
          "\\*Bookmark List\\*"
          "\\*Buffer List\\*"
          "Output\\*$"
          "\\*diff-hl\\*"
          vterm-mode
          treemacs-mode
          dap-server-log-mode
          dap-ui-sessions-mode
          dap-ui-breakpoints-ui-list-mode
          compilation-mode
          help-mode))
  (setq popper-window-height 30
        popper-mode-line ""
        popper-group-function #'(lambda () (safe-persp-name (get-current-persp))))
  :config
  (popper-mode +1))
;; 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

(provide 'configs-projects)
;;; projects.el ends here
