;;; early-init.el --- Emacs early-init -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Prevent package.el loading packages prior to their init-file loading.
(setq package-enable-at-startup nil)
(setq load-prefer-newer t)
(setq gc-cons-threshold (* 1024 1024 100))

(setq configs/user-config-dir (expand-file-name "~/.config"))

(when (getenv-internal "DEBUG")
  (setq init-file-debug t
        debug-on-error t))

(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-message nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

(setq ring-bell-function #'ignore)
(setq find-file-visit-truename t)
(setq confirm-nonexistent-file-or-buffer nil)
(setq confirm-kill-processes nil)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

(setq message-log-max 2000)
(setq warning-minimum-level :emergency)

(provide 'early-init)
;;; early-init.el ends here
