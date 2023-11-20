;;; init.el --- Emacs packages init -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(setq config-directory (expand-file-name "packages" user-emacs-directory))

;; load-packages

(load-file (expand-file-name "utils.el" config-directory))
(load-file (expand-file-name "evil.el" config-directory))
(load-file (expand-file-name "editor.el" config-directory))
(load-file (expand-file-name "minibuffer.el" config-directory))
(load-file (expand-file-name "programming.el" config-directory))
(load-file (expand-file-name "terminal.el" config-directory))
(load-file (expand-file-name "modes.el" config-directory))
(load-file (expand-file-name "lsp.el" config-directory))

;; end load packages

(elpaca-wait)

;; diminish mode-lines
(dolist (mode '(evil-collection-unimpaired-mode
                auto-revert-mode
                projectile-mode
                disable-mouse-mode
                company-mode))
  (diminish mode))

;; set mode configurations
;; add disable line-number hooks for the following modes
(dolist (mode '(vterm-mode-hook
		        ranger-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; use emacs-mode for the following modes
(dolist (mode '(vterm-mode
                ranger-mode
                elpaca-ui-mode
                message-mode
                dap-ui-breakpoints-ui-list-mode
                eglot-list-connections-mode))
  (add-to-list 'evil-emacs-state-modes mode))

;;Turns off elpaca-use-package-mode current declartion
;;Note this will cause the declaration to be interpreted immediately (not deferred).
;;Useful for configuring built-in emacs features.
(use-package emacs :elpaca nil :config (setq ring-bell-function #'ignore))

;; Don't install anything. Defer execution of BODY
(elpaca nil (message "deferred"))

(provide 'packages-init)

;;; init.el ends here
