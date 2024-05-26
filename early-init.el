;;; early-init.el --- Emacs early-init -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Prevent package.el loading packages prior to their init-file loading.
(setq package-enable-at-startup nil)
(setq load-prefer-newer t)
(setq gc-cons-threshold most-positive-fixnum)

(defvar +user-config-dir (expand-file-name "~/.config"))
(defvar +persp-state-default-directory (expand-file-name "var/perspective" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

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

(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

(setq-default truncate-lines t)
(setq-default line-spacing 0)
(setq-default indent-tabs-mode nil)
(setq-default visual-line-mode nil)
(setq-default use-file-dialog nil)
(setq-default show-help-function nil)

(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(menu-bar-mode -1)
(electric-pair-mode -1)
(winner-mode 1)
(global-eldoc-mode -1)

(setq message-log-max 2000)
(setq warning-minimum-level :emergency)

(defvar +fonts-fixed-pitch-face "SauceCodePro Nerd Font Mono")
(defvar +fonts-fixed-pitch-italic-face "SauceCodePro Nerd Font Mono")
(defvar +fonts-variable-pitch-face "SauceCodePro Nerd Font Propo")

(defvar +fonts-fixed-pitch-size 90)
(defvar +fonts-variable-pitch-size 90)
(defvar +fonts-tab-size 100)

(add-to-list
 'default-frame-alist
 `(font . ,(concat +fonts-fixed-pitch-face
                   (format "-%d" (/ +fonts-fixed-pitch-size 10)))))

(provide 'early-init)
;;; early-init.el ends here
