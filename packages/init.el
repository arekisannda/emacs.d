;; init.el --- Emacs packages init -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defvar user-packages-directory (expand-file-name "packages" user-emacs-directory))

;; load-packages

(load-file (expand-file-name "utils.el" user-packages-directory))
(load-file (expand-file-name "evil.el" user-packages-directory))
(load-file (expand-file-name "modes.el" user-packages-directory))
(load-file (expand-file-name "lsp.el" user-packages-directory))
(load-file (expand-file-name "org.el" user-packages-directory))
(load-file (expand-file-name "programming.el" user-packages-directory))
(load-file (expand-file-name "lang.el" user-packages-directory))
(load-file (expand-file-name "projects.el" user-packages-directory))
(load-file (expand-file-name "terminal.el" user-packages-directory))
(load-file (expand-file-name "minibuffer.el" user-packages-directory))
(load-file (expand-file-name "editor.el" user-packages-directory))

;; end load packages

(elpaca-wait)

;; popup configuration
(defun configs--enable-posframe ()
  "Use posframe."
  (add-hook 'flycheck-mode-hook 'flycheck-posframe-mode)
  (ace-window-posframe-mode 1)
  (company-posframe-mode 1)

  (require 'mozc-cand-posframe)
  (setq mozc-candidate-style 'posframe))

(defun configs--enable-popup ()
  "Use popup."
  (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))

(defun configs--get-popup-type ()
  "Retrieve popup dependency package."
  (if (display-graphic-p)
      'posframe
    'popup))

(defun configs--load-deferred-configurations ()
  "Load deferred package configurations."
  (let ((popup-type (configs--get-popup-type)))
    (cl-case popup-type
      (posframe (configs--enable-posframe))
      (popup (configs--enable-popup))
      (otherwise (message "Unable to configure popup")))))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame) (with-selected-frame frame (configs--load-deferred-configurations))))
  (configs--load-deferred-configurations))


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
                special-mode
                dap-ui-breakpoints-ui-list-mode
                eglot-list-connections-mode))
  (add-to-list 'evil-emacs-state-modes mode))

(provide 'packages-init)

;;; init.el ends here
