;;; early-init.el --- Emacs early-init -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Prevent package.el loading packages prior to their init-file loading.
(setq package-enable-at-startup nil)

;; Allows packages to be managed externally
(setq use-package-always-ensure nil)
;;
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer t)
;;
;; increase this early, decrease later on again
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq message-log-max 2000)
(setq warning-minimum-level :emergency)

(when (getenv-internal "DEBUG")
  (setq init-file-debug t
        debug-on-error t))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default inhibit-message nil
              inhibit-startup-screen t
              inhibit-startup-message t
              inhibit-startup-echo-area-message t

              minibuffer-message-timeout 0
              ring-bell-function #'ignore
              find-file-visit-truename t
              confirm-nonexistent-file-or-buffer nil
              confirm-kill-processes nil
              confirm-kill-emacs #'y-or-n-p
              auto-save-default t
              make-backup-files nil
              create-lockfiles nil
              custom-unlispify-tag-names nil

              fill-column 120
              visual-fill-column-width 120
              window-resize-pixelwise nil
              frame-resize-pixelwise nil
              hscroll-step 5
              scroll-step 5
              tab-width 4
              tab-bar-show nil
              tab-bar-separator ""
              fringe-indicator-alist nil
              bookmark-fringe-mark nil
              window-divider-default-places t
              window-divider-default-right-width 1
              window-divider-default-bottom-width 1
              mouse-avoidance-mode 'banish
              display-line-numbers-width 4
              display-line-numbers-widen t
              cursor-in-non-selected-windows nil
              split-height-threshold nil
              split-width-threshold 160
              parens-require-spaces nil

              truncate-lines t
              truncate-partial-width-windows nil
              line-spacing 0
              indent-tabs-mode nil
              visual-line-mode nil
              use-file-dialog nil
              use-dialog-box nil
              show-help-function nil)

(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(menu-bar-mode -1)
(electric-pair-mode -1)
(global-eldoc-mode -1)
(window-divider-mode 1)
(epa-file-enable)
(auth-source-pass-enable)

(defvar +fonts-fixed-pitch-face "SauceCodePro NFM")
(defvar +fonts-fixed-pitch-italic-face "SauceCodePro NFM")
(defvar +fonts-variable-pitch-face "SauceCodePro NFP")
(defvar +fonts-fixed-pitch-size 90)
(defvar +fonts-variable-pitch-size 90)
(defvar +fonts-tab-size 100)

(add-to-list
 'default-frame-alist
 `(font . ,(concat +fonts-fixed-pitch-face
                   (format " %d" (/ +fonts-fixed-pitch-size 10)))))

(set-fontset-font "fontset-default" 'han (font-spec :family "Source Han Sans"))
(set-fontset-font "fontset-default" 'kana (font-spec :family "Source Han Sans"))

(setq-default frame-title-format
              '((:eval (if init-file-debug (propertize "[DEBUG] " 'face '(:foreground "#ffffff" ))))
                "%F"
                (:eval (if tab-bar-mode (format ": %s" (cdr (assq 'name (tab-bar--current-tab))))))))

(unless init-file-debug (server-start))

(provide 'early-init)

;;; early-init.el ends here
