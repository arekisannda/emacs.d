;;; early-init.el --- Emacs early-init -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Prevent package.el loading packages prior to their init-file loading.
(setq package-enable-at-startup nil)
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
              auto-save-default nil
              make-backup-files nil
              create-lockfiles nil

              fill-column 100
              visual-fill-column-width 100
              window-resize-pixelwise t
              frame-resize-pixelwise t
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
(winner-mode -1)
(global-eldoc-mode -1)
(window-divider-mode 1)
(epa-file-enable)
(auth-source-pass-enable)

(setq-default frame-title-format
              '((:eval (if init-file-debug (propertize "[DEBUG] " 'face '(:foreground "#ffffff" ))))
                "%F"
                (:eval (if tab-bar-mode (format ": %s" (cdr (assq 'name (tab-bar--current-tab))))))))

;; setup elpaca package manager

;; elpaca 0.7 {{{
;;  (defvar elpaca-installer-version 0.7)
;;  (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
;;  (defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
;;  (defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
;;  (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.                  :ref nil :depth 1
;;                                :files (:defaults "elpaca-test.el" (:exclude "extensions"))
;;                                :build (:not elpaca--activate-package)))
;;  (let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
;;         (build (expand-file-name "elpaca/" elpaca-builds-directory))
;;         (order (cdr elpaca-order))
;;         (default-directory repo))
;;    (add-to-list 'load-path (if (file-exists-p build) build repo))
;;    (unless (file-exists-p repo)
;;      (make-directory repo t)
;;      (when (< emacs-major-version 28) (require 'subr-x))
;;      (condition-case-unless-debug err
;;          (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
;;                   ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
;;                                                   ,@(when-let ((depth (plist-get order :depth)))
;;                                                       (list (format "--depth=%d" depth) "--no-single-branch"))
;;                                                   ,(plist-get order :repo) ,repo))))
;;                   ((zerop (call-process "git" nil buffer t "checkout"
;;                                         (or (plist-get order :ref) "--"))))
;;                   (emacs (concat invocation-directory invocation-name))
;;                   ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
;;                                         "--eval" "(byte-recompile-directory \".\" 0 'force)")))
;;                   ((require 'elpaca))
;;                   ((elpaca-generate-autoloads "elpaca" repo)))
;;              (progn (message "%s" (buffer-string)) (kill-buffer buffer))
;;            (error "%s" (with-current-buffer buffer (buffer-string))))
;;        ((error) (warn "%s" err) (delete-directory repo 'recursive))))
;;    (unless (require 'elpaca-autoloads nil t)
;;      (require 'elpaca)
;;      (elpaca-generate-autoloads "elpaca" repo)
;;      (load "./elpaca-autoloads")))
;;  (add-hook 'after-init-hook #'elpaca-process-queues)
;;  (elpaca `(,@elpaca-order))
;; }}}

(defvar elpaca-installer-version 0.9)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode))
;; Assume :elpaca t unless otherwise specified.
(setq use-package-always-ensure t)
(setq elpaca-hide-initial-build nil)
(setq elpaca-hide-status-during-build nil)

(unless init-file-debug (server-start))

(provide 'early-init)

;;; early-init.el ends here
