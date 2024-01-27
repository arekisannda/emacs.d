;;; init.el --- Emacs configuration init -*- lexical-binding: t; origami-fold-style: triple-braces; -*-
;;; Commentary:

;;; Code:

;; config variables {{{
(defvar configs--debug-theme-enable nil
  "Flag to set to enable the debugging of theme.")

(defvar configs--fixed-pitch-font-face  "FiraMono Nerd Font Mono"
  "Fixed pitch font face.")

(defvar configs--variable-pitch-font-face "Fira Sans"
  "Variable pitch font face.")

(defvar configs--fixed-pitch-font-size 90
  "Fixed pitch font size.")

(defvar configs--variable-pitch-font-size 100
  "Variable pitch font size.")

(defvar configs--tab-font-size 100
  "Tab bar font size.")

(defvar configs--user-emacs-directory (expand-file-name "~/.config/emacs")
  "User Emacs directory.")

(setq-default custom-file (expand-file-name "custom.el" configs--user-emacs-directory))
;; }}}

;; setup elpaca package manager {{{
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" configs--user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
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
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
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
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t)
  (setq elpaca-hide-initial-build t)
  (setq elpaca-hide-status-during-build t))

(elpaca-wait)
;; }}}

(load-file (expand-file-name "utils/init.el" configs--user-emacs-directory))
(load-file (expand-file-name "configs/init.el" configs--user-emacs-directory))
(load-file (expand-file-name "keybinds/init.el" configs--user-emacs-directory))
(load custom-file)

(provide 'init)
;;; init.el ends here
