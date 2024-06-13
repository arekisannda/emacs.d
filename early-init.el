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

(custom-set-variables
 '(inhibit-message nil)
 '(inhibit-startup-screen t)
 '(inhibit-startup-message t)
 '(inhibit-startup-echo-area-message t)

 '(ring-bell-function #'ignore)
 '(find-file-visit-truename t)
 '(confirm-nonexistent-file-or-buffer nil)
 '(confirm-kill-processes nil)
 '(auto-save-default nil)
 '(make-backup-files nil)
 '(create-lockfiles nil)

 '(window-resize-pixelwise t)
 '(frame-resize-pixelwise t)
 '(hscroll-step 5)
 '(scroll-step 5)
 '(tab-width 4)
 '(tab-bar-separator "")
 '(fringe-indicator-alist nil)
 '(fringe-mode nil)

 '(truncate-lines t)
 '(line-spacing 0)
 '(indent-tabs-mode nil)
 '(visual-line-mode nil)
 '(use-file-dialog nil)
 '(use-dialog-box nil)
 '(show-help-function nil))

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
