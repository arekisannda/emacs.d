;;; projects.el --- Emeacs project management configurations -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; project management utils
(use-package project)

;; Update the frame name to include the current perspective
(use-package persp-mode
  :init
  (setq wg-morph-on nil)
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  :config
  (add-hook 'persp-before-deactivate-functions
            (defun +workspaces-save-tab-bar-data-h (_)
              (when (get-current-persp)
                (set-persp-parameter
                 'tab-bar-tabs (tab-bar-tabs)))))

  (add-hook 'persp-activated-functions
            (defun +workspaces-load-tab-bar-data-h (_)
              (tab-bar-tabs-set (persp-parameter 'tab-bar-tabs))
              (tab-bar--update-tab-bar-lines t)))

  (add-hook 'persp-before-save-state-to-file-functions
            (defun +workspaces-save-tab-bar-data-to-file-h (&rest _)
              (when (get-current-persp)
                (set-persp-parameter 'tab-bar-tabs (frameset-filter-tabs (tab-bar-tabs) nil nil t)))))

  (add-hook 'persp-activated-functions
            (defun +workspace-set-frame-name (_)
              (let ((current (safe-persp-name (get-current-persp))))
                (if (string= current "none")
                    (set-frame-name "main")
                  (set-frame-name current)))))

  (add-hook 'window-setup-hook #'(lambda () (persp-mode 1))))

(defun configs--popper-group-by-perspective ()
  "Return an identifier to group popups.

This returns the name of the perspective."
  (unless (fboundp 'get-current-persp)
    (user-error "Cannot find perspective name to group popups. \
Please install `persp-mode' or customize \
`popper-group-function'"))
  (safe-persp-name (get-current-persp)))

;; transient buffer management utils
(use-package popper
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
          "\\*elpaca-manager*\\*"
          "\\*elpaca-logs*\\*"
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
        popper-group-function #'configs--popper-group-by-perspective)
  :config
  (popper-mode +1))
;; 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

(provide 'packages-projects)

;;; projects.el ends here
