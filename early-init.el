;;; early-init.el  -*- lexical-binding: t; -*-

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
              window-resize-pixelwise nil
              frame-resize-pixelwise nil
              tab-width 4
              tab-bar-show nil
              tab-bar-separator ""
              window-divider-default-places t
              window-divider-default-right-width 1
              window-divider-default-bottom-width 1
              mouse-avoidance-mode 'banish
              display-line-numbers-type 'relative
              display-line-numbers-width 4
              display-line-numbers-widen t
              cursor-in-non-selected-windows nil
              split-height-threshold nil
              split-width-threshold 160
              parens-require-spaces nil
              resize-mini-windows 'grow-only
              max-mini-window-height 0.25
              read-quoted-char-radix 16

              mode-line-format nil
              left-margin-width 0
              right-margin-width 0
              fringe-indicator-alist nil
              left-fringe-width 16
              right-fringe-width 16
              bookmark-fringe-mark nil

              hscroll-step 1
              scroll-step 1
              scroll-preserve-screen-position t
              scroll-conservatively most-positive-fixnum
              scroll-margin 0
              maximum-scroll-margin 0.4
              scroll-error-top-bottom t
              switch-to-buffer-preserve-window-point t
              auto-window-vscroll nil

              truncate-lines t
              truncate-partial-width-windows nil
              line-spacing 0
              indent-tabs-mode nil
              visual-line-mode nil
              use-file-dialog nil
              use-dialog-box nil
              show-help-function nil

              set-mark-command-repeat-pop t
              bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t
              redisplay-skip-fontification-on-input t
              kill-do-not-save-duplicates t
              save-interprogram-paste-before-kill t
              reb-re-syntax 'string)

(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(menu-bar-mode -1)
(window-divider-mode 1)
(pixel-scroll-precision-mode 1)
(epa-file-enable)
(auth-source-pass-enable)

(defcustom +fonts-ckj-family "Source Han Sans"
  "Emacs CKJ font family."
  :type 'string
  :group 'basic-faces)

(defcustom +fonts-fixed-pitch-family "SauceCodePro NFM"
  "Emacs fixed-pitch font family."
  :type 'string
  :group 'basic-faces)

(defcustom +fonts-fixed-pitch-italic-family "SauceCodePro NFM"
  "Emacs fixed-pitch italic font family."
  :type 'string
  :group 'basic-faces)

(defcustom +fonts-variable-pitch-family "SauceCodePro NFP"
  "Emacs variable-pitch font family."
  :type 'string
  :group 'basic-faces)

(defcustom +fonts-fixed-pitch-size 90
  "Emacs fixed-pitch font size."
  :type 'integer
  :group 'basic-faces)

(defcustom +fonts-variable-pitch-size 90
  "Emacs variable-pitch font size."
  :type 'integer
  :group 'basic-faces)

(defcustom +fonts-tab-size 100
  "Emacs tab font size."
  :type 'integer
  :group 'basic-faces)

(defun +emacs-set-font ()
  (add-to-list
   'default-frame-alist
   `(font . ,(concat +fonts-fixed-pitch-family
                     (format " %d" (/ +fonts-fixed-pitch-size 10)))))

  (set-fontset-font "fontset-default" 'han (font-spec :family +fonts-ckj-family))
  (set-fontset-font "fontset-default" 'kana (font-spec :family +fonts-ckj-family)))

(+emacs-set-font)

(setq-default frame-title-format
              '((:eval (when init-file-debug "[DEBUG] "))
                (:eval (or (frame-parameter (selected-frame) 'prefix) "%F"))
                (:eval (when (and tab-bar-mode (activities-current))
                         (format " > %s" (cdr (assq 'name (tab-bar--current-tab))))))
                (:eval (when (and tab-bar-mode (activities-current))
                         (when-let ((ws (activities-workspaces-last (activities-current-workspace))))
                           (format " > %s" ws))))
                ))

(unless init-file-debug
  (setq message-log-max nil)
  (condition-case err
      (server-start)
    (error nil)))

(when init-file-debug
  (toggle-debug-on-error)
  (add-hook 'kill-emacs-hook
            (lambda ()
              (with-current-buffer "*Messages*"
                (write-region (point-min) (point-max) "/tmp/emacs-messages.log")))))
